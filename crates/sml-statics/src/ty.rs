//! Checking types.

use crate::error::{ErrorKind, Item};
use crate::types::{Ty, TyScheme, TyVarSrc};
use crate::util::{apply_bv, record};
use crate::{def, env::Cx, get_env::get_ty_info, info::TyEntry, st::St};

/// The mode for how we're checking this type.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Mode {
  /// The normal checking mode.
  Regular,
  /// We're checking the RHS of a `type` or `datatype` declaration.
  TyRhs,
}

pub(crate) fn get(
  st: &mut St,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  mode: Mode,
  ty: sml_hir::TyIdx,
) -> Ty {
  let ty = match ty {
    Some(x) => x,
    None => return Ty::None,
  };
  // NOTE: do not early return, since we add to the Info at the bottom.
  let mut ty_scheme = None::<TyScheme>;
  let mut def = None::<def::Def>;
  let ret = match &ars.ty[ty] {
    sml_hir::Ty::Hole => {
      st.err(ty, ErrorKind::TyHole);
      Ty::None
    }
    // @def(44)
    sml_hir::Ty::Var(v) => match cx.fixed.get(v) {
      None => {
        st.err(ty, ErrorKind::Undefined(Item::TyVar, v.as_name().clone()));
        Ty::None
      }
      Some(fv) => match (mode, fv.src()) {
        // regular mode allows all ty var types, and ty vars bound at types are always valid.
        (Mode::Regular, _) | (_, TyVarSrc::Ty) => Ty::FixedVar(fv.clone()),
        (Mode::TyRhs, TyVarSrc::Val) => {
          st.err(ty, ErrorKind::TyVarNotAllowedForTyRhs);
          Ty::None
        }
      },
    },
    // @def(45)
    sml_hir::Ty::Record(rows) => {
      let rows = record(st, ty.into(), rows, |st, _, ty| get(st, cx, ars, mode, ty));
      Ty::Record(rows)
    }
    // @def(46)
    sml_hir::Ty::Con(arguments, path) => {
      let ty_info = get_ty_info(&cx.env, path);
      for e in ty_info.errors {
        st.err(ty, e);
      }
      match ty_info.val {
        Some(ty_info) => {
          let want_len = ty_info.ty_scheme.bound_vars.len();
          if want_len == arguments.len() {
            ty_scheme = Some(ty_info.ty_scheme.clone());
            def = ty_info.def;
            let mut ret = ty_info.ty_scheme.ty.clone();
            let arguments: Vec<_> =
              arguments.iter().map(|&ty| get(st, cx, ars, mode, ty)).collect();
            apply_bv(&arguments, &mut ret);
            // NOTE: just because `ty` was a `sml_hir::Ty::Con` doesn't mean `ret` is ultimately a
            // `Ty::Con`. there could have been a type alias. e.g. `type unit = {}` (which indeed is
            // provided by the standard basis).
            ret
          } else {
            st.err(ty, ErrorKind::WrongNumTyArgs(want_len, arguments.len()));
            Ty::None
          }
        }
        None => Ty::None,
      }
    }
    // @def(47)
    sml_hir::Ty::Fn(param, res) => {
      let param = get(st, cx, ars, mode, *param);
      let res = get(st, cx, ars, mode, *res);
      Ty::fun(param, res)
    }
  };
  let ty_entry = TyEntry::new(ret.clone(), ty_scheme);
  st.info.insert(ty.into(), Some(ty_entry), def.into_iter().collect());
  ret
}
