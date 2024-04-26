//! Formatting utilities.

use std::fmt;

/// A type that when displayed could be a name for a **unutterable** type variable.
/// It will **not** be a name of an actual SML type variable that a user can type in real code.
#[derive(Debug)]
#[must_use]
pub struct TyVarName {
  equality: bool,
  idx: usize,
}

impl TyVarName {
  /// Returns a new one of these.
  pub fn new(equality: bool, idx: usize) -> Self {
    Self { equality, idx }
  }
}

impl fmt::Display for TyVarName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ticks = if self.equality { 2 } else { 1 };
    for c in std::iter::repeat('?').take(ticks).chain(idx_to_name(self.idx)) {
      write!(f, "{c}")?;
    }
    Ok(())
  }
}

fn idx_to_name(idx: usize) -> impl Iterator<Item = char> {
  let alpha = 26usize;
  let quot = idx / alpha;
  let rem = u8::try_from(idx % alpha).unwrap();
  let ch = char::from(b'a' + rem);
  std::iter::repeat(ch).take(quot + 1)
}
