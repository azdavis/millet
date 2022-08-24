use crate::error::{ErrorKind, Item};
use crate::get_env::get_ty_info;
use crate::info::TyEntry;
use crate::st::St;
use crate::types::{Cx, Def, Ty, TyScheme};
use crate::util::{apply_bv, record};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &sml_hir::Arenas, ty: sml_hir::TyIdx) -> Ty {
  let ty = match ty {
    Some(x) => x,
    None => return Ty::None,
  };
  // NOTE: do not early return, since we add to the Info at the bottom.
  let mut ty_scheme = None::<TyScheme>;
  let mut def = None::<Def>;
  let ret = match &ars.ty[ty] {
    sml_hir::Ty::Hole => {
      st.err(ty, ErrorKind::TyHole);
      Ty::None
    }
    // sml_def(44)
    sml_hir::Ty::Var(v) => match cx.fixed.get(v) {
      None => {
        st.err(ty, ErrorKind::Undefined(Item::TyVar, v.as_name().clone()));
        Ty::None
      }
      Some(fv) => Ty::FixedVar(fv.clone()),
    },
    // sml_def(45)
    sml_hir::Ty::Record(rows) => {
      let rows = record(st, rows, ty.into(), |st, _, ty| get(st, cx, ars, ty));
      Ty::Record(rows)
    }
    // sml_def(46)
    sml_hir::Ty::Con(args, path) => match get_ty_info(&cx.env, path) {
      Ok(ty_info) => {
        ty_scheme = Some(ty_info.ty_scheme.clone());
        def = ty_info.def;
        let want_len = ty_info.ty_scheme.bound_vars.len();
        let mut ret = Ty::None;
        if want_len == args.len() {
          let args: Vec<_> = args.iter().map(|&ty| get(st, cx, ars, ty)).collect();
          ret = ty_info.ty_scheme.ty.clone();
          apply_bv(&args, &mut ret)
        } else {
          st.err(ty, ErrorKind::WrongNumTyArgs(want_len, args.len()));
        }
        // NOTE: just because `ty` was a `sml_hir::Ty::Con` doesn't mean `ret` is ultimately a
        // `Ty::Con`. there could have been a type alias. e.g. `type unit = {}` (which indeed is
        // provided by the standard basis).
        ret
      }
      Err(e) => {
        st.err(ty, e);
        Ty::None
      }
    },
    // sml_def(47)
    sml_hir::Ty::Fn(param, res) => {
      let param = get(st, cx, ars, *param);
      let res = get(st, cx, ars, *res);
      Ty::fun(param, res)
    }
  };
  let ty_entry = TyEntry {
    ty: ret.clone(),
    ty_scheme,
  };
  st.info().insert(ty.into(), Some(ty_entry), def);
  ret
}
