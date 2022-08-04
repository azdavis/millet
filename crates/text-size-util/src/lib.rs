//! A thin wrapper around [`text_size`] to add some helper functions and types.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

pub use text_size::{TextLen, TextRange, TextSize};

/// A value located in a text file.
#[derive(Debug, Clone, Copy)]
pub struct WithRange<T> {
  /// The value.
  pub val: T,
  /// The location.
  pub range: text_size::TextRange,
}

impl<T> WithRange<T> {
  /// Wrap a new value with the location from `self`.
  pub fn wrap<U>(&self, val: U) -> WithRange<U> {
    WithRange {
      val,
      range: self.range,
    }
  }
}

/// Make a text size or panic. Panics if the usize overflows a u32.
pub fn mk_text_size(n: usize) -> TextSize {
  TextSize::try_from(n).expect("could not make text size")
}
