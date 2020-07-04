//! Check types.

use crate::ast::Ty as AstTy;
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::util::{get_env, tuple_lab};
use crate::statics::types::{Cx, Item, Result, State, StaticsError, Ty};
use std::collections::HashSet;

pub fn ck(cx: &Cx, st: &mut State, ty: &Located<AstTy<StrRef>>) -> Result<Ty> {
  let ret = match &ty.val {
    AstTy::TyVar(_) => {
      //
      return Err(ty.loc.wrap(StaticsError::Todo));
    }
    AstTy::Record(rows) => {
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut keys = HashSet::with_capacity(rows.len());
      for row in rows {
        let ty = ck(cx, st, &row.ty)?;
        if !keys.insert(row.lab.val) {
          return Err(row.lab.loc.wrap(StaticsError::DuplicateLabel(row.lab.val)));
        }
        ty_rows.push((row.lab.val, ty));
      }
      Ty::Record(ty_rows)
    }
    AstTy::Tuple(tys) => {
      let mut ty_rows = Vec::with_capacity(tys.len());
      for (idx, ty) in tys.iter().enumerate() {
        let ty = ck(cx, st, ty)?;
        let lab = tuple_lab(idx);
        ty_rows.push((lab, ty));
      }
      Ty::Record(ty_rows)
    }
    AstTy::TyCon(args, name) => {
      let env = get_env(cx, name)?;
      let ty_fcn = match env.ty_env.inner.get(&name.last.val) {
        None => {
          return Err(
            name
              .last
              .loc
              .wrap(StaticsError::Undefined(Item::Type, name.last.val)),
          )
        }
        // NOTE could avoid this clone if we separated datatypes from State
        Some(x) => x.ty_fcn(&st.datatypes).clone(),
      };
      if ty_fcn.ty_vars.len() != args.len() {
        let err = StaticsError::WrongNumTyArgs(ty_fcn.ty_vars.len(), args.len());
        return Err(ty.loc.wrap(err));
      }
      let mut new_args = Vec::with_capacity(ty_fcn.ty_vars.len());
      for ty in args {
        new_args.push(ck(cx, st, ty)?);
      }
      ty_fcn.apply_args(new_args)
    }
    AstTy::Arrow(arg, res) => {
      let arg = ck(cx, st, arg)?;
      let res = ck(cx, st, res)?;
      Ty::Arrow(arg.into(), res.into())
    }
  };
  Ok(ret)
}
