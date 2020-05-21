use crate::source_file::SourceFileId;

#[derive(Debug, PartialEq, Eq)]
pub struct Loc {
  pub file_id: SourceFileId,
  pub line: usize,
  pub col: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Located<T> {
  pub val: T,
  pub loc: Loc,
}

impl<'s, T> Located<T> {
  pub fn new(loc: Loc, val: T) -> Self {
    Self { loc, val }
  }
}
