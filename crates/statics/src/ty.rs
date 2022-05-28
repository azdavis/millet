use crate::error::ErrorKind;
use crate::st::St;
use crate::types::{Cx, Ty};
use crate::util::{apply_bv, get_env, record};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, ty: hir::TyIdx) -> Ty {
  let ty = match ty {
    Some(x) => x,
    None => return Ty::None,
  };
  match &ars.ty[ty] {
    hir::Ty::Var(v) => match cx.ty_vars.get(v) {
      None => {
        st.err(ty, ErrorKind::Undefined);
        Ty::None
      }
      Some(fv) => Ty::FixedVar(fv.clone()),
    },
    hir::Ty::Record(rows) => record(st, rows, ty, |st, _, ty| get(st, cx, ars, ty)),
    hir::Ty::Con(args, path) => {
      let env = match get_env(&cx.env, path.structures()) {
        Ok(x) => x,
        Err(_) => {
          st.err(ty, ErrorKind::Undefined);
          return Ty::None;
        }
      };
      let ty_info = match env.ty_env.get(path.last()) {
        Some(x) => x,
        None => {
          st.err(ty, ErrorKind::Undefined);
          return Ty::None;
        }
      };
      let want_len = ty_info.ty_scheme.vars.len();
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
    hir::Ty::Fn(param, res) => {
      let param = get(st, cx, ars, *param);
      let res = get(st, cx, ars, *res);
      Ty::fun(param, res)
    }
  }
}
