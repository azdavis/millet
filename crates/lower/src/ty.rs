use crate::common::{get_lab, get_path};
use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, ty: Option<ast::Ty>) -> hir::TyIdx {
  let ty = ty.and_then(|x| get_(cx, x)).unwrap_or(hir::Ty::None);
  cx.arenas.ty.alloc(ty)
}

fn get_(cx: &mut Cx, ty: ast::Ty) -> Option<hir::Ty> {
  let ret = match ty {
    ast::Ty::TyVarTy(ty) => hir::Ty::Var(hir::TyVar::new(ty.ty_var()?.text())),
    ast::Ty::RecordTy(ty) => hir::Ty::Record(
      ty.ty_rows()
        .filter_map(|x| Some((get_lab(x.lab()?)?, get(cx, x.ty()))))
        .collect(),
    ),
    ast::Ty::ConTy(ty) => {
      let path = get_path(ty.path()?)?;
      let args = ty
        .ty_seq()
        .into_iter()
        .flat_map(|x| x.ty_args())
        .map(|x| get(cx, x.ty()))
        .collect();
      hir::Ty::Con(path, args)
    }
    ast::Ty::TupleTy(ty) => hir::Ty::Record(
      ty.ty_stars()
        .enumerate()
        .map(|(idx, t)| (hir::Lab::Num(idx + 1), get(cx, t.ty())))
        .collect(),
    ),
    ast::Ty::FnTy(ty) => {
      let param = get(cx, ty.param());
      let res = get(cx, ty.res());
      hir::Ty::Fn(param, res)
    }
    ast::Ty::ParenTy(ty) => get_(cx, ty.ty()?)?,
  };
  Some(ret)
}
