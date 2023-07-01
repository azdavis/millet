//! Formatting utilities.

use std::fmt;

/// Returns a char iterator that when collected could be a name for a type variable.
pub fn ty_var_name(equality: bool, idx: usize) -> TyVarName {
  TyVarName { equality, idx }
}

/// A char iterator for a type variable name.
#[derive(Debug)]
#[must_use]
pub struct TyVarName {
  equality: bool,
  idx: usize,
}

impl fmt::Display for TyVarName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ticks = if self.equality { 2 } else { 1 };
    for c in std::iter::repeat('\'').take(ticks).chain(idx_to_name(self.idx)) {
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
