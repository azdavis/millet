//! Utilities for dealing with (collections of) source files.

use codespan_reporting::files::Files;

/// An opaque identifier for a source file.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceId(usize);

/// A source file.
pub struct Source {
  name: String,
  contents: String,
  new_lines: Vec<usize>,
}

impl Source {
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

  pub fn name(&self) -> &str {
    self.name.as_str()
  }

  pub fn as_bytes(&self) -> &[u8] {
    self.contents.as_bytes()
  }
}

/// A collection of all the source files.
pub struct SourceMap {
  files: Vec<Source>,
}

impl SourceMap {
  pub fn new() -> Self {
    Self { files: Vec::new() }
  }

  pub fn insert(&mut self, name: String, contents: String) {
    self.files.push(Source::new(name, contents));
  }

  pub fn iter(&self) -> Iter {
    Iter {
      files: &self.files,
      idx: 0,
    }
  }

  pub fn len(&self) -> usize {
    self.files.len()
  }
}

impl<'a> Files<'a> for SourceMap {
  type FileId = SourceId;
  type Name = &'a str;
  type Source = &'a str;

  fn name(&'a self, id: Self::FileId) -> Option<Self::Name> {
    let file = self.files.get(id.0)?;
    Some(file.name.as_str())
  }

  fn source(&'a self, id: Self::FileId) -> Option<Self::Source> {
    let file = self.files.get(id.0)?;
    Some(file.contents.as_str())
  }

  fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Option<usize> {
    let file = self.files.get(id.0)?;
    let ret = file
      .new_lines
      .iter()
      .position(|&x| byte_index <= x)
      .unwrap_or_else(|| file.new_lines.len());
    Some(ret)
  }

  fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<std::ops::Range<usize>> {
    let file = self.files.get(id.0)?;
    if line_index > file.new_lines.len() {
      return None;
    }
    let begin = if line_index == 0 {
      0
    } else {
      file.new_lines[line_index - 1] + 1
    };
    let end = if line_index == file.new_lines.len() {
      file.as_bytes().len()
    } else {
      file.new_lines[line_index]
    };
    Some(begin..end)
  }
}

/// An iterator of all the source files and their IDs.
pub struct Iter<'s> {
  files: &'s [Source],
  idx: usize,
}

impl<'s> Iterator for Iter<'s> {
  type Item = (SourceId, &'s Source);

  fn next(&mut self) -> Option<Self::Item> {
    let ret = Some((SourceId(self.idx), self.files.get(self.idx)?));
    self.idx += 1;
    ret
  }
}
