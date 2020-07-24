//! Type realizations.
//!
//! This is here, and not in types.rs, basically just because types.rs is already a big file.

use crate::statics::types::{Sym, Ty, TyFcn};
use std::collections::HashMap;

/// A mapping from symbols to type functions.
#[derive(Debug, Default)]
pub struct TyRealization {
  inner: HashMap<Sym, TyFcn>,
}

impl TyRealization {
  /// Insert the key, val pair into this.
  pub fn insert(&mut self, key: Sym, val: TyFcn) {
    assert!(self.inner.insert(key, val).is_none());
  }

  /// Applies this to a `Ty`.
  pub fn apply_to_ty(&self, ty: &mut Ty) {
    match ty {
      Ty::Var(_) => {}
      Ty::Record(rows) => {
        for ty in rows.values_mut() {
          self.apply_to_ty(ty);
        }
      }
      Ty::Arrow(lhs, rhs) => {
        self.apply_to_ty(lhs);
        self.apply_to_ty(rhs);
      }
      Ty::Ctor(args, sym) => {
        for arg in args.iter_mut() {
          self.apply_to_ty(arg);
        }
        if let Some(ty_fcn) = self.inner.get(sym) {
          *ty = ty_fcn.apply_args(args.clone());
        }
      }
    }
  }
}
