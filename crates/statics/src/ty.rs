use crate::st::St;
use crate::types::Ty;
use crate::util::record;

pub(crate) fn get(st: &mut St, ars: &hir::Arenas, ty: hir::TyIdx) -> Ty {
  match ars.ty[ty] {
    hir::Ty::None => Ty::None,
    hir::Ty::Var(_) => todo!(),
    hir::Ty::Record(ref rows) => record(st, rows, |st, _, ty| get(st, ars, ty)),
    hir::Ty::Con(_, _) => todo!(),
    hir::Ty::Fn(param, res) => {
      let param = get(st, ars, param);
      let res = get(st, ars, res);
      Ty::Fn(param.into(), res.into())
    }
  }
}
