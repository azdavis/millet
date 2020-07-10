//! Utilities for checking ASTs.

use crate::ast::Long;
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::types::{
  Env, Error, Item, Result, State, Subst, SymTys, Ty, TyEnv, TyInfo, TyScheme, ValInfo,
};
use std::collections::BTreeMap;

/// Replaces all type variables, in the type in this TyScheme, which are bound by that same
/// TyScheme, with fresh type variables, and returns that type.
pub fn instantiate(st: &mut State, ty_scheme: &TyScheme, loc: Loc) -> Ty {
  let mut subst = Subst::default();
  match &ty_scheme.overload {
    None => {
      for &tv in ty_scheme.ty_vars.iter() {
        subst.insert(tv, Ty::Var(st.new_ty_var(tv.equality)));
      }
    }
    Some(overloads) => {
      // NOTE it might be better to have TyScheme be an enum to make these illegal states impossible
      // to represent in the types. But then again, overloading is uncommon since only the standard
      // library is permitted to do it. In fact, the fact that only the standard library may do
      // overloading, and that all overloaded operators are similar in that every one of them has
      // only one overloaded type variable which is meant to be "the same" within a single
      // instantiation, leads to this slightly hacky implementation.
      let mut iter = ty_scheme.ty_vars.iter().copied();
      let tv = iter.next().unwrap();
      assert!(iter.next().is_none());
      assert!(!overloads.is_empty());
      assert!(!tv.equality);
      let new_tv = st.new_ty_var(false);
      subst.insert(tv, Ty::Var(new_tv));
      st.overload.push((loc, new_tv, overloads.clone()));
    }
  }
  let mut ty = ty_scheme.ty.clone();
  ty.apply(&subst);
  ty
}

/// Mutates the TyScheme, which has no type variables, to bind all free type variables in the type
/// in this TyScheme, except for those type variables which are free in the TyEnv.
pub fn generalize(ty_env: &TyEnv, sym_tys: &SymTys, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.ty_vars.is_empty());
  assert!(ty_scheme.overload.is_none());
  ty_scheme.ty_vars = ty_scheme
    .ty
    .free_ty_vars()
    .difference(&ty_env.free_ty_vars(sym_tys))
    .copied()
    .collect();
}

pub fn get_env<'env>(mut env: &'env Env, long: &Long<StrRef>) -> Result<&'env Env> {
  for &s in long.structures.iter() {
    env = match env.str_env.get(&s.val) {
      None => return Err(s.loc.wrap(Error::Undefined(Item::Structure, s.val))),
      Some(x) => x,
    }
  }
  Ok(env)
}

pub fn get_val_info(env: &Env, name: Located<StrRef>) -> Result<&ValInfo> {
  match env.val_env.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Value, name.val))),
    Some(val_info) => Ok(val_info),
  }
}

pub fn get_ty_info(env: &Env, name: Located<StrRef>) -> Result<&TyInfo> {
  match env.ty_env.inner.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Type, name.val))),
    Some(ty_info) => Ok(ty_info),
  }
}

pub fn env_ins<T>(map: &mut BTreeMap<StrRef, T>, key: Located<StrRef>, val: T) -> Result<()> {
  if map.insert(key.val, val).is_some() {
    Err(key.loc.wrap(Error::Redefined(key.val)))
  } else {
    Ok(())
  }
}

pub fn env_merge<T>(
  lhs: &mut BTreeMap<StrRef, T>,
  rhs: BTreeMap<StrRef, T>,
  loc: Loc,
) -> Result<()> {
  for (key, val) in rhs {
    env_ins(lhs, loc.wrap(key), val)?;
  }
  Ok(())
}
