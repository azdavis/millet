//! Check types.

use crate::ast::Ty as AstTy;
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::util::{get_env, get_ty_info, tuple_lab};
use crate::statics::types::{Cx, Error, Result, SymTys, Ty};
use std::collections::BTreeMap;

pub fn ck(cx: &Cx, sym_tys: &SymTys, ty: &Located<AstTy<StrRef>>) -> Result<Ty> {
  // SML Definition (48) is handled by the parser
  match &ty.val {
    // SML Definition (44)
    AstTy::TyVar(_) => Err(ty.loc.wrap(Error::Todo("type variables"))),
    // SML Definition (45)
    AstTy::Record(rows) => {
      let mut ty_rows = BTreeMap::new();
      // SML Definition (49)
      for row in rows {
        let ty = ck(cx, sym_tys, &row.val)?;
        if ty_rows.insert(row.lab.val, ty).is_some() {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
      }
      Ok(Ty::Record(ty_rows))
    }
    // SML Definition Appendix A - tuples are sugar for records
    AstTy::Tuple(tys) => {
      let mut ty_rows = BTreeMap::new();
      for (idx, ty) in tys.iter().enumerate() {
        let ty = ck(cx, sym_tys, ty)?;
        assert!(ty_rows.insert(tuple_lab(idx), ty).is_none());
      }
      Ok(Ty::Record(ty_rows))
    }
    // SML Definition (46)
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
    // SML Definition (47)
    AstTy::Arrow(arg, res) => {
      let arg = ck(cx, sym_tys, arg)?;
      let res = ck(cx, sym_tys, res)?;
      Ok(Ty::Arrow(arg.into(), res.into()))
    }
  }
}
