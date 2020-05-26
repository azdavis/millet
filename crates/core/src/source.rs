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
}

/// A generic wrapper for some value which was ultimately derived from some
/// location in the source.
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
  bytes: Vec<u8>,
}

impl SourceFile {
  pub fn as_bytes(&self) -> &[u8] {
    &self.bytes
  }
}

/// A collection of all the source files.
#[derive(Default)]
pub struct SourceMap {
  files: Vec<SourceFile>,
}

impl SourceMap {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn add(&mut self, name: String, bytes: Vec<u8>) {
    self.files.push(SourceFile { name, bytes });
  }

  pub fn iter(&self) -> impl Iterator<Item = (SourceFileId, &SourceFile)> {
    self
      .files
      .iter()
      .enumerate()
      .map(|(id, sf)| (SourceFileId(id), sf))
  }

  pub fn report<T>(&self, err: Located<T>)
  where
    T: std::error::Error,
  {
    let file = self.files.get(err.loc.file_id.0).unwrap();
  }
}
