//! Types and fundamental operations for static analysis.

#![allow(clippy::too_many_lines, clippy::single_match_else)]

mod data;

pub mod def;
pub mod disallow;
pub mod display;
pub mod env;
pub mod equality;
pub mod generalize;
pub mod info;
pub mod item;
pub mod mode;
pub mod overload;
pub mod sym;
pub mod ty;
pub mod unify;
pub mod util;

/// The overall mutable state when typechecking.
///
/// Called `St` as a short for `State`, as is common, but also conveniently contains exactly two
/// fields: the `Syms` (S) and the `Tys` (T).
#[derive(Debug, Default, Clone)]
pub struct St {
  /// The syms.
  pub syms: sym::Syms,
  /// The tys.
  pub tys: ty::Tys,
}
