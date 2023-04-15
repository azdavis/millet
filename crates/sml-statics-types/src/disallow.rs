//! See [`Disallow`].

use crate::item::Item;
use std::fmt;

/// A way in which something is not allowed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Disallow {
  /// It was directly disallowed by name, not transitively.
  Directly,
}

impl fmt::Display for Disallow {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Disallow::Directly => f.write_str("directly"),
    }
  }
}

/// An error when trying to disallow a path.
#[derive(Debug)]
pub enum Error {
  /// We tried to disallow something that doesn't exist.
  Undefined(Item, str_util::Name),
  /// We tried to disallow something that was already disallowed.
  Already(Disallow),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Self::Undefined(item, name) => write!(f, "undefined {item}: `{name}`"),
      Self::Already(d) => write!(f, "already disallowed {d}"),
    }
  }
}
