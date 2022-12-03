//! Bound type variables, as in type schemes and type functions.

use crate::fmt_util;

/// Basically a de Bruijn index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct BoundTyVar(idx::Idx);

impl BoundTyVar {
  pub(crate) fn index_into<T>(self, xs: &[T]) -> &T {
    xs.get(self.0.to_usize()).unwrap()
  }

  pub(crate) fn name(self, equality: bool) -> fmt_util::TyVarName {
    fmt_util::ty_var_name(equality, self.0.to_usize())
  }

  pub(crate) fn iter_for<I, T>(xs: I) -> impl Iterator<Item = (Self, T)>
  where
    I: Iterator<Item = T>,
  {
    xs.enumerate().map(|(i, x)| (Self(idx::Idx::new(i)), x))
  }

  pub(crate) fn add_to_binder<T, F>(binder: &mut Vec<T>, f: F) -> Self
  where
    F: FnOnce(Self) -> T,
  {
    let ret = Self(idx::Idx::new(binder.len()));
    let val = f(ret);
    binder.push(val);
    ret
  }
}
