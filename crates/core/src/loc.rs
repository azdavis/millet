use crate::source_file::SourceFileId;

#[derive(Debug, PartialEq, Eq)]
pub struct Loc {
  file_id: SourceFileId,
  line: usize,
  col: usize,
}

impl Loc {
  pub fn new(file_id: SourceFileId, line: usize, col: usize) -> Self {
    Self { file_id, line, col }
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Located<T> {
  val: T,
  loc: Loc,
}

impl<'s, T> Located<T> {
  pub fn new(loc: Loc, val: T) -> Self {
    Self { loc, val }
  }
}
