use crate::cx::Cx;
use crate::types::Ty;
use crate::util::record;

pub(crate) fn get(cx: &mut Cx, ars: &hir::Arenas, ty: hir::TyIdx) -> Ty {
  match ars.ty[ty] {
    hir::Ty::None => Ty::None,
    hir::Ty::Var(_) => todo!(),
    hir::Ty::Record(ref rows) => record(cx, rows, |cx, _, ty| get(cx, ars, ty)),
    hir::Ty::Con(_, _) => todo!(),
    hir::Ty::Fn(param, res) => {
      let param = get(cx, ars, param);
      let res = get(cx, ars, res);
      Ty::Fn(param.into(), res.into())
    }
  }
}
