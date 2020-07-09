//! Check types.

use crate::ast::Ty as AstTy;
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::util::{get_env, get_ty_info, tuple_lab};
use crate::statics::types::{Cx, Error, Result, SymTys, Ty};
use std::collections::HashSet;

pub fn ck(cx: &Cx, sym_tys: &SymTys, ty: &Located<AstTy<StrRef>>) -> Result<Ty> {
  match &ty.val {
    AstTy::TyVar(_) => Err(ty.loc.wrap(Error::Todo("type variables"))),
    AstTy::Record(rows) => {
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut keys = HashSet::with_capacity(rows.len());
      for row in rows {
        let ty = ck(cx, sym_tys, &row.ty)?;
        if !keys.insert(row.lab.val) {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
        ty_rows.push((row.lab.val, ty));
      }
      Ok(Ty::Record(ty_rows))
    }
    AstTy::Tuple(tys) => {
      let mut ty_rows = Vec::with_capacity(tys.len());
      for (idx, ty) in tys.iter().enumerate() {
        let ty = ck(cx, sym_tys, ty)?;
        let lab = tuple_lab(idx);
        ty_rows.push((lab, ty));
      }
      Ok(Ty::Record(ty_rows))
    }
    AstTy::TyCon(args, name) => {
      let env = get_env(&cx.env, name)?;
      let ty_fcn = get_ty_info(env, name.last)?.ty_fcn(&sym_tys);
      if ty_fcn.ty_vars.len() != args.len() {
        let err = Error::WrongNumTyArgs(ty_fcn.ty_vars.len(), args.len());
        return Err(ty.loc.wrap(err));
      }
      let mut new_args = Vec::with_capacity(ty_fcn.ty_vars.len());
      for ty in args {
        new_args.push(ck(cx, sym_tys, ty)?);
      }
      Ok(ty_fcn.apply_args(new_args))
    }
    AstTy::Arrow(arg, res) => {
      let arg = ck(cx, sym_tys, arg)?;
      let res = ck(cx, sym_tys, res)?;
      Ok(Ty::Arrow(arg.into(), res.into()))
    }
  }
}
