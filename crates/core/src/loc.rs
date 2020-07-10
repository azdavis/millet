//! Source code locations.

use std::fmt;

/// A range in the source. The start is inclusive, the end is not inclusive.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Loc {
  start: usize,
  end: usize,
}

impl Loc {
  /// Returns a new Loc. Panics if start >= end.
  pub fn new(start: usize, end: usize) -> Self {
    assert!(start < end);
    Self { start, end }
  }

  /// Returns a new Loc that begins where self began and ends where other ends. Panics if other
  /// starts before self starts.
  pub fn span(self, other: Self) -> Self {
    assert!(self.start <= other.start);
    Self {
      start: self.start,
      end: other.end,
    }
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
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Located<T> {
  /// The value.
  pub val: T,
  /// The location of the value.
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
