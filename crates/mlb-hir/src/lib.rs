//! HIR for ML Basis.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

use text_size_util::WithRange;

#[derive(Debug, Clone)]
pub enum BasDec {
  Basis(WithRange<str_util::Name>, Box<BasExp>),
  Open(WithRange<str_util::Name>),
  Local(Box<BasDec>, Box<BasDec>),
  Export(Namespace, WithRange<str_util::Name>, WithRange<str_util::Name>),
  Seq(Vec<BasDec>),
  Path(paths::PathId, PathKind),
}

impl BasDec {
  /// Returns a sequence of decs.
  ///
  /// # Panics
  ///
  /// If there was an internal error.
  #[must_use]
  pub fn seq(mut decs: Vec<Self>) -> Self {
    if decs.len() == 1 {
      decs.pop().unwrap()
    } else {
      Self::Seq(decs)
    }
  }
}

#[derive(Debug, Clone)]
pub enum BasExp {
  Bas(BasDec),
  Name(WithRange<str_util::Name>),
  Let(BasDec, Box<BasExp>),
}

#[derive(Debug, Clone, Copy)]
pub enum Namespace {
  Structure,
  Signature,
  Functor,
}

#[derive(Debug, Clone, Copy)]
pub enum PathKind {
  Sml,
  Mlb,
}
