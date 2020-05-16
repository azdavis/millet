pub struct Loc {
  pub line: usize,
  pub col: usize,
}

pub struct Located<T> {
  pub val: T,
  pub loc: Loc,
}

impl<T> Located<T> {
  pub fn new(loc: Loc, val: T) -> Self {
    Self { loc, val }
  }
}
