//! Check types.

use crate::ast::{Label, Ty as AstTy};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::util::{get_env, get_ty_info};
use crate::statics::types::{Cx, Error, Item, Result, Ty, Tys};
use std::collections::BTreeMap;

pub fn ck(cx: &Cx, tys: &Tys, ty: &Located<AstTy<StrRef>>) -> Result<Ty> {
  // SML Definition (48) is handled by the parser
  match &ty.val {
    // SML Definition (44)
    AstTy::TyVar(tv) => match cx.ty_vars.get(tv) {
      None => Err(ty.loc.wrap(Error::Undefined(Item::TyVar, tv.name))),
      Some(x) => Ok(Ty::Var(*x)),
    },
    // SML Definition (45)
    AstTy::Record(rows) => {
      let mut ty_rows = BTreeMap::new();
      // SML Definition (49)
      for row in rows {
        let ty = ck(cx, tys, &row.val)?;
        if ty_rows.insert(row.lab.val, ty).is_some() {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
      }
      Ok(Ty::Record(ty_rows))
    }
    // SML Definition Appendix A - tuples are sugar for records
    AstTy::Tuple(ts) => {
      let mut ty_rows = BTreeMap::new();
      for (idx, ty) in ts.iter().enumerate() {
        let ty = ck(cx, tys, ty)?;
        assert!(ty_rows.insert(Label::tuple(idx), ty).is_none());
      }
      Ok(Ty::Record(ty_rows))
    }
    // SML Definition (46)
    AstTy::TyCon(args, name) => {
      let env = get_env(&cx.env, name)?;
      let ty_fcn = &get_ty_info(tys, env, name.last)?.ty_fcn;
      if ty_fcn.ty_vars.len() != args.len() {
        let err = Error::WrongNumTyArgs(ty_fcn.ty_vars.len(), args.len());
        return Err(ty.loc.wrap(err));
      }
      let mut new_args = Vec::with_capacity(ty_fcn.ty_vars.len());
      for ty in args {
        new_args.push(ck(cx, tys, ty)?);
      }
      Ok(ty_fcn.apply_args(new_args))
    }
    // SML Definition (47)
    AstTy::Arrow(arg, res) => {
      let arg = ck(cx, tys, arg)?;
      let res = ck(cx, tys, res)?;
      Ok(Ty::Arrow(arg.into(), res.into()))
    }
  }
}
