//! Utilities for dealing with (collections of) source files.

use millet_core::loc::Loc;

/// An opaque identifier for a source file.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceFileId(usize);

/// A source file.
pub struct SourceFile {
  name: String,
  contents: String,
  new_lines: Vec<usize>,
}

impl SourceFile {
  fn new(name: String, contents: String) -> Self {
    let new_lines = contents
      .as_bytes()
      .iter()
      .enumerate()
      .filter_map(|(idx, &b)| if b == b'\n' { Some(idx) } else { None })
      .collect();
    Self {
      name,
      contents,
      new_lines,
    }
  }

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
    self.files.push(SourceFile::new(name, contents));
  }

  pub fn iter(&self) -> Iter {
    Iter {
      files: &self.files,
      idx: 0,
    }
  }

  pub fn get_ctx(&self, id: SourceFileId, loc: Loc) -> SourceCtx<'_> {
    let file = &self.files[id.0];
    let bs = file.as_bytes();
    let start = loc.into_range().start;
    let (idx, end) = match file.new_lines.iter().position(|&x| start < x) {
      Some(idx) => (idx, file.new_lines[idx]),
      None => (file.new_lines.len(), bs.len()),
    };
    let (col_num, start) = if idx == 0 {
      (start + 1, 0)
    } else {
      let prev = file.new_lines[idx - 1];
      (start - prev, prev + 1)
    };
    SourceCtx {
      file_name: &file.name,
      line: std::str::from_utf8(&bs[start..end]).unwrap(),
      line_num: idx + 1,
      col_num,
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
