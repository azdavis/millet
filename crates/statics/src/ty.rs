use crate::error::{ErrorKind, Item};
use crate::st::St;
use crate::types::{Cx, Ty};
use crate::util::{apply_bv, get_ty_info, record};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, ty: hir::TyIdx) -> Ty {
  let ty = match ty {
    Some(x) => x,
    None => return Ty::None,
  };
  match &ars.ty[ty] {
    // sml_def(44)
    hir::Ty::Var(v) => match cx.ty_vars.get(v) {
      None => {
        st.err(ty, ErrorKind::Undefined(Item::TyVar, v.clone().into_name()));
        Ty::None
      }
      Some(fv) => Ty::FixedVar(fv.clone()),
    },
    // sml_def(45)
    hir::Ty::Record(rows) => Ty::Record(record(st, rows, ty, |st, _, ty| get(st, cx, ars, ty))),
    // sml_def(46)
    hir::Ty::Con(args, path) => {
      let ty_info = match get_ty_info(&cx.env, path) {
        Ok(x) => x,
        Err(e) => {
          st.err(ty, e);
          return Ty::None;
        }
      };
      let want_len = ty_info.ty_scheme.bound_vars.len();
      let mut ret = Ty::None;
      if want_len == args.len() {
        let args: Vec<_> = args.iter().map(|&ty| get(st, cx, ars, ty)).collect();
        ret = ty_info.ty_scheme.ty.clone();
        apply_bv(&args, &mut ret)
      } else {
        st.err(ty, ErrorKind::WrongNumTyArgs(want_len, args.len()));
      }
      // NOTE: just because `ty` was a `hir::Ty::Con` doesn't mean `ret` is ultimately a `Ty::Con`.
      // there could have been a type alias. e.g. `type unit = {}` (which indeed is provided by the
      // standard basis).
      ret
    }
    // sml_def(47)
    hir::Ty::Fn(param, res) => {
      let param = get(st, cx, ars, *param);
      let res = get(st, cx, ars, *res);
      Ty::fun(param, res)
    }
  }
}
