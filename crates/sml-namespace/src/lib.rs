//! See [`Namespace`].

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::fmt;

/// A namespace for "structure-level" items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Namespace {
  /// `structure`
  Structure,
  /// `signature`
  Signature,
  /// `functor`
  Functor,
}

impl fmt::Display for Namespace {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Namespace::Structure => f.write_str("structure"),
      Namespace::Signature => f.write_str("signature"),
      Namespace::Functor => f.write_str("functor"),
    }
  }
}
