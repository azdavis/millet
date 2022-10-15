//! Utilities for strings.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::borrow::Borrow;
use std::fmt;

pub use smol_str::SmolStr;

/// An immutable, somewhat cheaply clone-able, non-empty string.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(SmolStr);

impl Name {
  /// Returns a new `Name`.
  ///
  /// # Panics
  ///
  /// If `s` is empty.
  pub fn new<S>(s: S) -> Self
  where
    S: Into<SmolStr>,
  {
    let s: SmolStr = s.into();
    assert!(!s.is_empty());
    Self(s)
  }

  /// Returns this as a string slice.
  #[must_use]
  pub fn as_str(&self) -> &str {
    self.0.as_str()
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl Borrow<str> for Name {
  fn borrow(&self) -> &str {
    self.as_str()
  }
}
