use crate::error::ErrorKind;
use crate::st::St;
use crate::types::{Cx, Ty};
use crate::util::{get_env, record};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, ty: hir::TyIdx) -> Ty {
  match &ars.ty[ty] {
    hir::Ty::None => Ty::None,
    hir::Ty::Var(v) => match cx.ty_vars.get(v) {
      None => {
        st.err(ErrorKind::Undefined);
        Ty::None
      }
      Some(fv) => Ty::FixedVar(fv.clone()),
    },
    hir::Ty::Record(rows) => record(st, rows, |st, _, ty| get(st, cx, ars, ty)),
    hir::Ty::Con(args, path) => {
      let env = match get_env(&cx.env, path) {
        Ok(x) => x,
        Err(_) => {
          st.err(ErrorKind::Undefined);
          return Ty::None;
        }
      };
      let sym = match env.ty_env.get(path.last()) {
        Some(x) => *x,
        None => {
          st.err(ErrorKind::Undefined);
          return Ty::None;
        }
      };
      Ty::Con(args.iter().map(|&ty| get(st, cx, ars, ty)).collect(), sym)
    }
    hir::Ty::Fn(param, res) => {
      let param = get(st, cx, ars, *param);
      let res = get(st, cx, ars, *res);
      Ty::Fn(param.into(), res.into())
    }
  }
}
