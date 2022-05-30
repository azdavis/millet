use crate::common::{get_lab, get_path};
use crate::util::Cx;
use syntax::ast::{self, AstPtr};

pub(crate) fn get(cx: &mut Cx, ty: Option<ast::Ty>) -> hir::TyIdx {
  let ty = ty?;
  let ptr = AstPtr::new(&ty);
  let ret = match ty {
    ast::Ty::TyVarTy(ty) => hir::Ty::Var(hir::TyVar::new(ty.ty_var()?.text())),
    ast::Ty::RecordTy(ty) => hir::Ty::Record(
      ty.ty_rows()
        .filter_map(|row| Some((get_lab(row.lab()?)?, get(cx, row.ty()))))
        .collect(),
    ),
    ast::Ty::ConTy(ty) => {
      let path = get_path(ty.path()?)?;
      hir::Ty::Con(
        ty.ty_seq()
          .into_iter()
          .flat_map(|x| x.ty_args())
          .map(|x| get(cx, x.ty()))
          .collect(),
        path,
      )
    }
    ast::Ty::OneArgConTy(ty) => {
      let path = get_path(ty.path()?)?;
      hir::Ty::Con(vec![get(cx, ty.ty())], path)
    }
    ast::Ty::TupleTy(ty) => hir::Ty::Record(
      std::iter::once(ty.ty())
        .chain(ty.star_tys().map(|x| x.ty()))
        .enumerate()
        .map(|(idx, t)| (hir::Lab::tuple(idx), get(cx, t)))
        .collect(),
    ),
    ast::Ty::FnTy(ty) => hir::Ty::Fn(get(cx, ty.param()), get(cx, ty.res())),
    // sml_def(48)
    ast::Ty::ParenTy(ty) => return get(cx, ty.ty()),
  };
  cx.ty(ret, ptr)
}

pub(crate) fn var_seq(tvs: Option<ast::TyVarSeq>) -> Vec<hir::TyVar> {
  tvs
    .into_iter()
    .flat_map(|x| x.ty_var_args())
    .filter_map(|x| x.ty_var())
    .map(|tok| hir::TyVar::new(tok.text()))
    .collect()
}
