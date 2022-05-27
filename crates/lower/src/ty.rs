use crate::common::{get_lab, get_path};
use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, ty: Option<ast::Ty>) -> hir::TyIdx {
  let ret = match ty? {
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
    ast::Ty::TupleTy(ty) => hir::Ty::Record(
      ty.ty_stars()
        .enumerate()
        .map(|(idx, t)| (hir::Lab::tuple(idx), get(cx, t.ty())))
        .collect(),
    ),
    ast::Ty::FnTy(ty) => hir::Ty::Fn(get(cx, ty.param()), get(cx, ty.res())),
    ast::Ty::ParenTy(ty) => return get(cx, ty.ty()),
  };
  cx.ty(ret)
}

pub(crate) fn var_seq(tvs: Option<ast::TyVarSeq>) -> Vec<hir::TyVar> {
  tvs
    .into_iter()
    .flat_map(|x| x.ty_var_args())
    .filter_map(|x| x.ty_var())
    .map(|tok| hir::TyVar::new(tok.text()))
    .collect()
}
