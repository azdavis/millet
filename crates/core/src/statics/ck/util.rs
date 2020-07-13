//! Utilities for checking ASTs.
//!
//! There is some overlap between this module and `types.rs`. If a function operating on the types
//! in `types.rs` is used in multiple other modules, and doesn't need access to internals in
//! `types.rs`, and doesn't make sense as a method on a type, it should live here.

use crate::ast::Long;
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::types::{
  Cx, Env, Error, Item, Result, State, Subst, Ty, TyEnv, TyInfo, TyScheme, ValInfo,
};
use crate::token::TyVar as AstTyVar;
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
      for ty in overloads {
        match ty {
          Ty::Ctor(args, sym) => {
            assert!(args.is_empty());
            assert!(sym.is_base());
          }
          _ => unreachable!(),
        }
      }
      let new_tv = st.new_ty_var(false);
      subst.insert(tv, Ty::Var(new_tv));
      st.overload.insert(new_tv, (loc, overloads.clone()));
    }
  }
  let mut ty = ty_scheme.ty.clone();
  ty.apply(&subst);
  ty
}

/// Mutates the TyScheme, which has no type variables, to bind all free type variables in the type
/// in this TyScheme, except for those type variables which are either free in the TyEnv, or are
/// overloaded type variables as noted by the State.
pub fn generalize(st: &State, ty_env: &TyEnv, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.ty_vars.is_empty());
  assert!(ty_scheme.overload.is_none());
  // could just be `ty_scheme.apply` by the above assert.
  ty_scheme.ty.apply(&st.subst);
  let ty_env_ty_vars = ty_env.free_ty_vars(&st.sym_tys);
  for tv in ty_scheme.ty.free_ty_vars() {
    if ty_env_ty_vars.contains(&tv) || st.overload.contains_key(&tv) {
      continue;
    }
    ty_scheme.ty_vars.push(tv);
  }
}

/// Returns `Ok(e)` iff `env` contains the environment `e` after traversing the `StrEnv`s of `env`
/// as directed by `long.structures`.
pub fn get_env<'env>(mut env: &'env Env, long: &Long<StrRef>) -> Result<&'env Env> {
  for &s in long.structures.iter() {
    env = match env.str_env.get(&s.val) {
      None => return Err(s.loc.wrap(Error::Undefined(Item::Struct, s.val))),
      Some(x) => x,
    }
  }
  Ok(env)
}

/// Returns `Ok(vi)` iff `env` contains `vi` in its `ValEnv`.
pub fn get_val_info(env: &Env, name: Located<StrRef>) -> Result<&ValInfo> {
  match env.val_env.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Val, name.val))),
    Some(val_info) => Ok(val_info),
  }
}

/// Returns `Ok(ti)` iff `env` contains `ti` in its `TyEnv`.
pub fn get_ty_info(env: &Env, name: Located<StrRef>) -> Result<&TyInfo> {
  match env.ty_env.inner.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Ty, name.val))),
    Some(ty_info) => Ok(ty_info),
  }
}

/// Insert the `key`, `val` pair into `map`. Returns `Ok(())` iff the key was not already in this
/// map.
pub fn env_ins<T>(map: &mut BTreeMap<StrRef, T>, key: Located<StrRef>, val: T) -> Result<()> {
  if map.insert(key.val, val).is_some() {
    Err(key.loc.wrap(Error::Redefined(key.val)))
  } else {
    Ok(())
  }
}

/// Merges `rhs` into `lhs`. Returns `Ok(()) iff there exists no key in `rhs` that was already in
/// `lhs`.
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

pub fn add_ty_vars(cx: &mut Cx, st: &mut State, ty_vars: &[Located<AstTyVar<StrRef>>]) {
  for tv in ty_vars {
    cx.ty_vars.insert(tv.val, st.new_ty_var(tv.val.equality));
  }
}
