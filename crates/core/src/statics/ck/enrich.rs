//! Check whether an environment enriches another environment.

// TODO remove when done
#![allow(unused)]

use crate::statics::types::{Env, Result, SymTys, TyFcn, TyInfo, ValEnv, ValInfo};

/// Returns Ok(()) iff lhs enriches rhs as per the Definition. TODO this will emit different errors
/// based on hash map iter order. Use BTreeMap instead?
pub fn ck(sym_tys: &SymTys, lhs: &Env, rhs: &Env) -> Result<()> {
  for (name, rhs) in rhs.str_env.iter() {
    match lhs.str_env.get(name) {
      None => todo!("missing a struct"),
      Some(lhs) => ck(sym_tys, lhs, rhs)?,
    }
  }
  for (name, rhs) in rhs.ty_env.inner.iter() {
    match lhs.ty_env.inner.get(name) {
      None => todo!("missing a type"),
      Some(lhs) => ck_ty_info(sym_tys, lhs, rhs)?,
    }
  }
  for (name, rhs) in rhs.val_env.iter() {
    match lhs.val_env.get(name) {
      None => todo!("missing a value"),
      Some(lhs) => ck_val_info(sym_tys, lhs, rhs)?,
    }
  }
  Ok(())
}

fn ck_val_info(sym_tys: &SymTys, lhs: &ValInfo, rhs: &ValInfo) -> Result<()> {
  todo!()
}

fn ck_ty_info(sym_tys: &SymTys, lhs: &TyInfo, rhs: &TyInfo) -> Result<()> {
  ck_ty_fcn_eq(lhs.ty_fcn(sym_tys), rhs.ty_fcn(sym_tys))?;
  let rhs_val_env = match rhs {
    TyInfo::Alias(_) => return Ok(()),
    TyInfo::Sym(sym) => &sym_tys.get(sym).unwrap().val_env,
  };
  if rhs_val_env.is_empty() {
    return Ok(());
  }
  let lhs_val_env = match lhs {
    TyInfo::Alias(_) => todo!("lhs empty rhs non-empty"),
    TyInfo::Sym(sym) => &sym_tys.get(sym).unwrap().val_env,
  };
  ck_val_env_eq(lhs_val_env, rhs_val_env)
}

fn ck_val_env_eq(lhs: &ValEnv, rhs: &ValEnv) -> Result<()> {
  todo!()
}

fn ck_ty_fcn_eq(lhs: &TyFcn, rhs: &TyFcn) -> Result<()> {
  todo!()
}
