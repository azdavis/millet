//! Utilities for checking ASTs.

use crate::ast::{Label, Long};
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::types::{
  Cx, Datatypes, Env, Error, Item, Result, State, Subst, Ty, TyEnv, TyScheme, ValInfo,
};
use std::collections::HashMap;
use std::convert::TryInto as _;

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

pub fn generalize(ty_env: &TyEnv, dts: &Datatypes, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.ty_vars.is_empty());
  assert!(ty_scheme.overload.is_none());
  ty_scheme.ty_vars = ty_scheme
    .ty
    .free_ty_vars()
    .difference(&ty_env.free_ty_vars(dts))
    .copied()
    .collect();
}

pub fn get_env<'cx>(cx: &'cx Cx, long: &Long<StrRef>) -> Result<&'cx Env> {
  let mut ret = &cx.env;
  for &s in long.structures.iter() {
    ret = match ret.str_env.get(&s.val) {
      None => return Err(s.loc.wrap(Error::Undefined(Item::Structure, s.val))),
      Some(x) => x,
    }
  }
  Ok(ret)
}

pub fn get_val_info(env: &Env, name: Located<StrRef>) -> Result<&ValInfo> {
  match env.val_env.get(&name.val) {
    None => Err(name.loc.wrap(Error::Undefined(Item::Value, name.val))),
    Some(val_info) => Ok(val_info),
  }
}

pub fn tuple_lab(idx: usize) -> Label {
  Label::Num((idx + 1).try_into().unwrap())
}

pub fn env_ins<T>(map: &mut HashMap<StrRef, T>, key: Located<StrRef>, val: T) -> Result<()> {
  if map.insert(key.val, val).is_some() {
    Err(key.loc.wrap(Error::Redefined(key.val)))
  } else {
    Ok(())
  }
}

pub fn env_merge<T>(lhs: &mut HashMap<StrRef, T>, rhs: HashMap<StrRef, T>, loc: Loc) -> Result<()> {
  for (key, val) in rhs {
    env_ins(lhs, loc.wrap(key), val)?;
  }
  Ok(())
}
