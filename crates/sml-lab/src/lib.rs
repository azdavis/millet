//! [`Lab`], a label, as for a record or tuple.

use std::fmt;
use str_util::Name;

/// A record/tuple label.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lab {
  /// A named label.
  Name(Name),
  /// A numeric label.
  Num(usize),
}

impl fmt::Display for Lab {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Name(name) => name.fmt(f),
      Self::Num(n) => n.fmt(f),
    }
  }
}

impl Lab {
  /// Return the numeric label for one greater than the number passed.
  #[must_use]
  pub fn tuple(idx: usize) -> Self {
    Self::Num(idx + 1)
  }
}
