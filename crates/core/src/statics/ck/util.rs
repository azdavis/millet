//! Utilities for checking ASTs.
//!
//! There is some overlap between this module and `types.rs`. If a function operating on the types
//! in `types.rs` is used in multiple other modules, and doesn't need access to internals in
//! `types.rs`, and doesn't make sense as a method on a type, it should live here.

use crate::ast::Long;
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::types::{
  Cx, Env, Error, Item, Result, State, Subst, Sym, Ty, TyInfo, TyScheme, Tys, ValInfo,
};
use crate::token::TyVar as AstTyVar;
use std::collections::BTreeMap;
use std::collections::HashSet;

/// Replaces all type variables, in the type in this TyScheme, which are bound by that same
/// TyScheme, with fresh type variables, and returns that type.
pub fn instantiate(st: &mut State, ty_scheme: &TyScheme) -> Ty {
  let mut subst = Subst::default();
  match &ty_scheme.overload {
    None => {
      for &tv in ty_scheme.ty_vars.iter() {
        // we cannot `assert!(!st.subst.is_bound(&tv))` because e.g. in `datatype 'a t = A of 'a`
        // the type variable 'a is bound.
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
      assert!(!st.subst.is_bound(&tv));
      for sym in overloads {
        assert!(sym.is_base());
      }
      let new_tv = st.new_ty_var(false);
      subst.insert(tv, Ty::Var(new_tv));
      st.subst.insert_overloaded(new_tv, overloads.clone());
    }
  }
  let mut ty = ty_scheme.ty.clone();
  ty.apply(&subst);
  ty
}

/// First, this marks all the type variables given by `ty_vars` (and `cx.ty_vars` which maps the AST
/// ty vars to statics ty vars) as no longer bound in the `Subst` in the `State`.
///
/// Then, this mutates the `TyScheme`, which upon entry, binds no type variables, to bind all free
/// type variables in the `Ty` in the `TyScheme`, except for those type variables which are:
///
/// - free in the `TyEnv` in the `Cx`, or
/// - are overloaded type variables as noted by the `Subst`, or
/// - are actually bound as noted by the `Subst`.
pub fn generalize(
  cx: &Cx,
  st: &mut State,
  ty_vars: &[Located<AstTyVar<StrRef>>],
  ty_scheme: &mut TyScheme,
) {
  assert!(ty_scheme.ty_vars.is_empty());
  assert!(ty_scheme.overload.is_none());
  // could just be `ty_scheme.apply` by the above assert.
  ty_scheme.ty.apply(&st.subst);
  for tv in ty_vars {
    let tv = cx.ty_vars.get(&tv.val).unwrap();
    // though the type variable is no longer bound by the `Subst`, it ought to be bound by the
    // `TyScheme`.
    st.subst.remove_bound(tv);
  }
  let ty_env_ty_vars = cx.env.ty_env.free_ty_vars(&st.tys);
  for tv in ty_scheme.ty.free_ty_vars() {
    if ty_env_ty_vars.contains(&tv) || st.subst.is_overloaded(&tv) || st.subst.is_bound(&tv) {
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

/// Returns `Ok(vi)` iff the `ValEnv` of `env` maps `name` to `vi`.
pub fn get_val_info(env: &Env, name: Located<StrRef>) -> Result<&ValInfo> {
  match env.val_env.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Val, name.val))),
    Some(val_info) => Ok(val_info),
  }
}

/// Returns `Ok(ti)` iff the `TyEnv` of `env` maps `name` to `sym` and `tys` maps `sym` to `ti`.
pub fn get_ty_info<'t>(tys: &'t Tys, env: &Env, name: Located<StrRef>) -> Result<&'t TyInfo> {
  let sym = get_ty_sym(env, name)?;
  Ok(tys.get(&sym).unwrap())
}

/// Returns `Ok(sym)` iff the `TyEnv` of `env` maps `name` to `sym`.
pub fn get_ty_sym(env: &Env, name: Located<StrRef>) -> Result<Sym> {
  match env.ty_env.inner.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Ty, name.val))),
    Some(sym) => Ok(*sym),
  }
}

/// Insert the `key`, `val` pair into `map`. Returns `Ok(())` iff the key was not already in this
/// map.
pub fn env_ins<T>(
  map: &mut BTreeMap<StrRef, T>,
  key: Located<StrRef>,
  val: T,
  item: Item,
) -> Result<()> {
  if map.insert(key.val, val).is_some() {
    Err(key.loc.wrap(Error::Duplicate(item, key.val)))
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
  item: Item,
) -> Result<()> {
  for (key, val) in rhs {
    env_ins(lhs, loc.wrap(key), val, item)?;
  }
  Ok(())
}

/// Add new statics ty vars based on the user-written ty vars to the Cx.
pub fn insert_ty_vars(
  cx: &mut Cx,
  st: &mut State,
  ty_vars: &[Located<AstTyVar<StrRef>>],
) -> Result<()> {
  let mut set = HashSet::new();
  for tv in ty_vars {
    if !set.insert(tv.val.name) {
      return Err(tv.loc.wrap(Error::Duplicate(Item::TyVar, tv.val.name)));
    }
    let new_tv = st.new_ty_var(tv.val.equality);
    cx.ty_vars.insert(tv.val, new_tv);
    st.subst.insert_bound(new_tv);
  }
  Ok(())
}
