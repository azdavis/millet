use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, ty: Option<ast::Ty>) -> hir::TyIdx {
  let ty = ty.and_then(|x| get_(cx, x)).unwrap_or(hir::Ty::None);
  cx.arenas.ty.alloc(ty)
}

fn get_(_: &mut Cx, ty: ast::Ty) -> Option<hir::Ty> {
  match ty {
    ast::Ty::TyVarTy(_) => todo!(),
    ast::Ty::RecordTy(_) => todo!(),
    ast::Ty::ConTy(_) => todo!(),
    ast::Ty::TupleTy(_) => todo!(),
    ast::Ty::FnTy(_) => todo!(),
    ast::Ty::ParenTy(_) => todo!(),
  }
}
