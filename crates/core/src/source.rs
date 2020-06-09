//! Utilities for dealing with source code.

/// A point location in the source.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Loc {
  file_id: SourceFileId,
  line: usize,
  col: usize,
}

impl Loc {
  pub fn new(file_id: SourceFileId, line: usize, col: usize) -> Self {
    Self { file_id, line, col }
  }

  pub fn wrap<T>(self, val: T) -> Located<T> {
    Located { val, loc: self }
  }
}

/// A generic wrapper for some value which was ultimately derived from some
/// location in the source.
#[derive(Debug, PartialEq, Eq)]
pub struct Located<T> {
  pub val: T,
  pub loc: Loc,
}

/// An opaque identifier for a source file.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceFileId(usize);

#[cfg(test)]
impl SourceFileId {
  pub fn new(id: usize) -> Self {
    Self(id)
  }
}

/// A source file.
pub struct SourceFile {
  name: String,
  contents: String,
}

impl SourceFile {
  pub fn as_bytes(&self) -> &[u8] {
    self.contents.as_bytes()
  }
}

/// A collection of all the source files.
pub struct SourceMap {
  files: Vec<SourceFile>,
}

pub struct SourceCtx<'a> {
  pub line_num: usize,
  pub col_num: usize,
  pub file_name: &'a str,
  pub line: &'a str,
}

impl SourceMap {
  pub fn new() -> Self {
    Self { files: Vec::new() }
  }

  pub fn insert(&mut self, name: String, contents: String) {
    self.files.push(SourceFile { name, contents });
  }

  pub fn iter(&self) -> Iter {
    Iter {
      files: &self.files,
      idx: 0,
    }
  }

  pub fn get_ctx(&self, loc: Loc) -> SourceCtx<'_> {
    let file = &self.files[loc.file_id.0];
    let bs = file.as_bytes();
    let mut line = 1;
    let mut start: Option<usize> = if loc.line == 1 { Some(0) } else { None };
    let mut end: Option<usize> = None;
    for (i, &b) in bs.iter().enumerate() {
      if b != b'\n' {
        continue;
      }
      if line == loc.line {
        end = Some(i);
        break;
      }
      line += 1;
      if line == loc.line {
        start = Some(i + 1);
      }
    }
    let start = start.unwrap();
    let end = end.unwrap_or_else(|| bs.len());
    let line = std::str::from_utf8(&bs[start..end]).unwrap();
    SourceCtx {
      file_name: &file.name,
      line,
      line_num: loc.line,
      col_num: loc.col,
    }
  }
}

/// An iterator of all the source files and their IDs.
pub struct Iter<'s> {
  files: &'s [SourceFile],
  idx: usize,
}

impl<'s> Iterator for Iter<'s> {
  type Item = (SourceFileId, &'s SourceFile);

  fn next(&mut self) -> Option<Self::Item> {
    let ret = Some((SourceFileId(self.idx), self.files.get(self.idx)?));
    self.idx += 1;
    ret
  }
}
