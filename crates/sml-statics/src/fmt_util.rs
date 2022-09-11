use std::fmt;

/// returns a char iterator that when collected could be a name for a type variable.
pub(crate) fn ty_var_name(equality: bool, idx: usize) -> TyVarName {
  TyVarName { equality, idx }
}

pub(crate) struct TyVarName {
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

pub(crate) fn idx_to_name(idx: usize) -> impl Iterator<Item = char> {
  let alpha = (b'z' - b'a') as usize;
  let quot = idx / alpha;
  let rem = idx % alpha;
  let ch = char::from((rem as u8) + b'a');
  std::iter::repeat(ch).take(quot + 1)
}
