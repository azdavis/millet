//! See [`Disallow`]

use std::fmt;

/// A way in which something is not allowed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Disallow {
  /// TODO remove
  #[allow(dead_code)]
  Directly,
}

impl fmt::Display for Disallow {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Disallow::Directly => f.write_str("directly"),
    }
  }
}
