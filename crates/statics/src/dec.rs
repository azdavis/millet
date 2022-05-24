use crate::st::St;
use crate::types::{Cx, Env};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, env: &mut Env, dec: hir::DecIdx) {
  match ars.dec[dec] {
    hir::Dec::Val(_, _) => todo!(),
    hir::Dec::Ty(_) => todo!(),
    hir::Dec::Datatype(_) => todo!(),
    hir::Dec::DatatypeCopy(_, _) => todo!(),
    hir::Dec::Abstype(_, _) => todo!(),
    hir::Dec::Exception(_) => todo!(),
    hir::Dec::Local(_, _) => todo!(),
    hir::Dec::Open(_) => todo!(),
    hir::Dec::Seq(ref decs) => {
      for &dec in decs {
        get(st, cx, ars, env, dec);
      }
    }
  }
}
