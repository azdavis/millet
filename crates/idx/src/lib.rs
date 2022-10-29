//! See [`Idx`].

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

/// An index type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Idx(usize);

impl Idx {
  /// Returns a new `Idx` for the usize.
  #[must_use]
  pub const fn new(n: usize) -> Self {
    Self(n)
  }

  /// Converts this back into a usize.
  #[must_use]
  pub const fn to_usize(self) -> usize {
    self.0
  }
}
