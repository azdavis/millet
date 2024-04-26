//! Checking types.

use crate::{error::ErrorKind, util::record};
use crate::{get_env::get_ty_info, info::TyEntry, st::St};
use sml_statics_types::ty::{Ty, TyData, TyScheme, TyVarSrc};
use sml_statics_types::{env::Cx, item::Item, util::apply_bv};

/// The mode for how we're checking this type.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Mode {
  /// The normal checking mode.
  Regular,
  /// We're checking the RHS of a `type` or `datatype` declaration.
  TyRhs,
}

pub(crate) fn get(
  st: &mut St<'_>,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  mode: Mode,
  ty: sml_hir::TyIdx,
) -> Ty {
  let Some(ty) = ty else { return Ty::NONE };
  // NOTE: do not early return, since we add to the Info at the bottom.
  let mut ty_scheme = None::<TyScheme>;
  let ret = match &ars.ty[ty] {
    sml_hir::Ty::Hole => {
      st.err(ty, ErrorKind::TyHole);
      Ty::NONE
    }
    // @def(44)
    sml_hir::Ty::Var(v) => match cx.fixed.get(v) {
      None => {
        st.err(ty, ErrorKind::Undefined(Item::TyVar, v.clone().into_name()));
        Ty::NONE
      }
      Some(fv) => {
        let TyData::FixedVar(fv_data) = st.syms_tys.tys.data(*fv) else {
          unreachable!("not a fixed var")
        };
        match (mode, fv_data.src) {
          // regular mode allows all ty var types, and ty vars bound at types are always valid.
          (Mode::Regular, _) | (_, TyVarSrc::Ty) => {}
          (Mode::TyRhs, TyVarSrc::Val) => st.err(ty, ErrorKind::TyVarNotAllowedForTyRhs),
        }
        *fv
      }
    },
    // @def(45)
    sml_hir::Ty::Record(rows) => {
      let rows = record(st, ty.into(), rows, |st, _, ty| get(st, cx, ars, mode, ty));
      st.syms_tys.tys.record(rows)
    }
    // @def(46)
    sml_hir::Ty::Con(arguments, path) => {
      let ty_info = get_ty_info(&cx.env, path);
      for e in ty_info.disallow {
        st.err(ty, e.into());
      }
      match ty_info.val {
        Ok(ty_info) => {
          let want_len = ty_info.ty_scheme.bound_vars.len();
          if want_len == arguments.len() {
            ty_scheme = Some(ty_info.ty_scheme.clone());
            let defs = st.info.entries.defs.ty.entry(ty).or_default();
            defs.extend(ty_info.defs.iter().copied());
            let mut ret = ty_info.ty_scheme.ty;
            let subst: Vec<_> = arguments.iter().map(|&ty| get(st, cx, ars, mode, ty)).collect();
            apply_bv(&mut st.syms_tys.tys, &subst, &mut ret);
            // NOTE: just because `ty` was a `sml_hir::Ty::Con` doesn't mean `ret` is ultimately a
            // `Ty::Con`. there could have been a type alias. e.g. `type unit = {}` (which indeed is
            // provided by the standard basis).
            ret
          } else {
            st.err(ty, ErrorKind::WrongNumTyArgs(want_len, arguments.len()));
            Ty::NONE
          }
        }
        Err(e) => {
          st.err(ty, e.into());
          Ty::NONE
        }
      }
    }
    // @def(47)
    sml_hir::Ty::Fn(param, res) => {
      let param = get(st, cx, ars, mode, *param);
      let res = get(st, cx, ars, mode, *res);
      st.syms_tys.tys.fun(param, res)
    }
  };
  st.info.entries.tys.ty.insert(ty, TyEntry::new(ret, ty_scheme));
  ret
}
