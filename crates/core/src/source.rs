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
pub struct SourceMap<W> {
  files: Vec<SourceFile>,
  writer: W,
}

impl<W> SourceMap<W> {
  pub fn new(writer: W) -> Self {
    Self {
      files: Vec::new(),
      writer,
    }
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
}

impl<W> SourceMap<W>
where
  W: std::io::Write,
{
  pub fn report<T>(&mut self, err: Located<T>) -> std::io::Result<()>
  where
    T: std::error::Error,
  {
    let file = &self.files[err.loc.file_id.0];
    let loc = err.loc;
    writeln!(self.writer, "error: {}", err.val)?;
    writeln!(self.writer, " --> {}:{}:{}", file.name, loc.line, loc.col)?;
    writeln!(self.writer, "  |")?;
    write!(self.writer, "  | ")?;
    self.writer.write(get_line(file.as_bytes(), loc))?;
    writeln!(self.writer)?;
    write!(self.writer, "  | ")?;
    for _ in 0..loc.col {
      write!(self.writer, " ")?;
    }
    writeln!(self.writer, "^")?;
    write!(self.writer, "  |")?;
    Ok(())
  }
}

fn get_line(bs: &[u8], loc: Loc) -> &[u8] {
  let mut line = 1;
  let mut start: Option<usize> = None;
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
      start = Some(i);
    }
  }
  &bs[start.unwrap()..end.unwrap()]
}
