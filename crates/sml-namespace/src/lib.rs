//! Different namespaces for various SML language definition constructs.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::fmt;

/// A namespace for "module-level" items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Module {
  /// `structure`
  Structure,
  /// `signature`
  Signature,
  /// `functor`
  Functor,
}

impl fmt::Display for Module {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Module::Structure => f.write_str("structure"),
      Module::Signature => f.write_str("signature"),
      Module::Functor => f.write_str("functor"),
    }
  }
}
