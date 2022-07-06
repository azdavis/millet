//! A thin wrapper around [`text_size`] to add some helper functions and types.

pub use text_size::{TextLen, TextRange, TextSize};

#[derive(Debug, Clone, Copy)]
pub struct Located<T> {
  pub val: T,
  pub range: text_size::TextRange,
}

impl<T> Located<T> {
  pub fn wrap<U>(&self, val: U) -> Located<U> {
    Located {
      val,
      range: self.range,
    }
  }
}

/// Make a text size or panic. Panics if the usize overflows a u32.
pub fn mk_text_size(n: usize) -> TextSize {
  TextSize::try_from(n).expect("could not make text size")
}
