//! See [`Idx`].

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

/// An index type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Idx(u32);

impl Idx {
  /// Returns a new `Idx` for the usize.
  #[must_use]
  pub fn new(n: usize) -> Self {
    Self(n.try_into().expect("couldn't convert to Idx"))
  }

  #[doc(hidden)]
  #[must_use]
  pub const fn new_u32(n: u32) -> Self {
    Self(n)
  }

  /// Converts this back into a usize.
  #[must_use]
  pub fn to_usize(self) -> usize {
    self.0.try_into().expect("couldn't convert from Idx")
  }
}
