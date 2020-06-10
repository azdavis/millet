//! Utilities for dealing with locations in source code.

use std::fmt;

/// A range in the source. The start is inclusive, the end is not inclusive.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Loc {
  start: usize,
  end: usize,
}

impl Loc {
  /// Returns a new Loc.
  pub fn new(start: usize, end: usize) -> Self {
    Self { start, end }
  }

  /// Wraps a value in a Loc.
  pub fn wrap<T>(self, val: T) -> Located<T> {
    Located { val, loc: self }
  }
}

impl From<Loc> for std::ops::Range<usize> {
  fn from(val: Loc) -> std::ops::Range<usize> {
    val.start..val.end
  }
}

/// A generic wrapper for some value which was ultimately derived from some
/// location in the source.
#[derive(PartialEq, Eq)]
pub struct Located<T> {
  pub val: T,
  pub loc: Loc,
}

impl<T> fmt::Debug for Located<T>
where
  T: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.val.fmt(f)
  }
}
