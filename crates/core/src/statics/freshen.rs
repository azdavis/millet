//! Freshen up a signature with new symbols.

use crate::loc::Loc;
use crate::statics::types::{Env, Sig, State, Sym, Ty, Tys, ValEnv};
use std::collections::HashMap;

pub fn get(st: &mut State, loc: Loc, mut sig: Sig) -> Env {
  let subst: Subst = sig
    .ty_names
    .iter()
    .map(|&old| (old, st.new_sym(loc.wrap(old.name()))))
    .collect();
  get_env(&mut st.tys, &subst, &mut sig.env);
  sig.env
}

type Subst = HashMap<Sym, Sym>;

fn get_env(tys: &mut Tys, subst: &Subst, env: &mut Env) {
  for env in env.str_env.values_mut() {
    get_env(tys, subst, env);
  }
  for old in env.ty_env.inner.values_mut() {
    let mut ty_info = tys.get(old).clone();
    get_ty(subst, &mut ty_info.ty_fcn.ty);
    get_val_env(subst, &mut ty_info.val_env);
    let new = *subst.get(old).unwrap();
    tys.insert(new, ty_info);
    *old = new;
  }
  get_val_env(subst, &mut env.val_env);
}

fn get_val_env(subst: &Subst, val_env: &mut ValEnv) {
  for val_info in val_env.values_mut() {
    get_ty(subst, &mut val_info.ty_scheme.ty);
  }
}

fn get_ty(subst: &Subst, ty: &mut Ty) {
  match ty {
    Ty::Var(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values_mut() {
        get_ty(subst, ty);
      }
    }
    Ty::Arrow(lhs, rhs) => {
      get_ty(subst, lhs);
      get_ty(subst, rhs);
    }
    Ty::Ctor(args, sym) => {
      for arg in args.iter_mut() {
        get_ty(subst, arg);
      }
      match subst.get(sym) {
        None => {}
        Some(new) => *sym = *new,
      }
    }
  }
}
