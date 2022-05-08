//! Type realizations.
//!
//! In the Definition, a type realization is just a mapping from TyName to TyFcn. In Millet, it is
//! that and a bit more: you may also map TyName to TyName. (Note that in Millet, TyName is
//! implemented as a `Sym`). This is useful when generating new type names as a result of signature
//! ascription or functor application.

use crate::types::{Env, Sym, Ty, TyFcn, Tys, ValEnv};
use std::collections::HashMap;

/// A mapping from symbols to type functions.
#[derive(Debug, Default)]
pub struct TyRealization {
  inner: HashMap<Sym, Out>,
}

/// The output of a subst, i.e. what a Sym maps to.
#[derive(Debug)]
enum Out {
  TyFcn(TyFcn),
  Sym(Sym),
}

impl TyRealization {
  /// Inserts the mapping `key => val` into this.
  pub fn insert_ty_fcn(&mut self, key: Sym, val: TyFcn) {
    assert!(self.inner.insert(key, Out::TyFcn(val)).is_none());
  }

  /// Inserts the mapping `key => val` into this.
  pub fn insert_sym(&mut self, key: Sym, val: Sym) {
    assert!(self.inner.insert(key, Out::Sym(val)).is_none());
  }

  /// Applies this to an `Env`.
  pub fn get_env(&self, tys: &mut Tys, env: &mut Env) {
    for env in env.str_env.values_mut() {
      self.get_env(tys, env);
    }
    for old in env.ty_env.inner.values_mut() {
      match self.inner.get(old) {
        None => continue,
        Some(Out::TyFcn(..)) => unreachable!(),
        Some(&Out::Sym(new)) => {
          let mut ty_info = tys.get(old).clone();
          self.get_ty(&mut ty_info.ty_fcn.ty);
          self.get_val_env(&mut ty_info.val_env);
          tys.insert(new, ty_info);
          *old = new;
        }
      }
    }
    self.get_val_env(&mut env.val_env);
  }

  fn get_val_env(&self, val_env: &mut ValEnv) {
    for val_info in val_env.values_mut() {
      self.get_ty(&mut val_info.ty_scheme.ty);
    }
  }

  /// Applies this to a `Ty`.
  pub fn get_ty(&self, ty: &mut Ty) {
    match ty {
      Ty::Var(_) => {}
      Ty::Record(rows) => {
        for ty in rows.values_mut() {
          self.get_ty(ty);
        }
      }
      Ty::Arrow(lhs, rhs) => {
        self.get_ty(lhs);
        self.get_ty(rhs);
      }
      Ty::Ctor(args, sym) => {
        for arg in args.iter_mut() {
          self.get_ty(arg);
        }
        if let Some(out) = self.inner.get(sym) {
          match out {
            Out::TyFcn(ty_fcn) => *ty = ty_fcn.apply_args(args.clone()),
            Out::Sym(new) => *sym = *new,
          }
        }
      }
    }
  }
}
