//! Error reporting to the console.

use millet_core::source::{Located, SourceFileId, SourceMap};

/// A reporter of errors.
pub struct Reporter<W> {
  writer: W,
}

impl<W> Reporter<W> {
  pub fn new(writer: W) -> Self {
    Self { writer }
  }
}

impl<W> Reporter<W>
where
  W: std::io::Write,
{
  pub fn report<T>(
    &mut self,
    map: &SourceMap,
    file_id: SourceFileId,
    err: Located<T>,
  ) -> std::io::Result<()>
  where
    T: std::error::Error,
  {
    let ctx = map.get_ctx(file_id, err.loc);
    let err = err.val;
    writeln!(self.writer, "error: {}", err)?;
    writeln!(
      self.writer,
      " --> {}:{}:{}",
      ctx.file_name, ctx.line_num, ctx.col_num
    )?;
    writeln!(self.writer, "  |")?;
    write!(self.writer, "  | ")?;
    self.writer.write(ctx.line.as_bytes())?;
    writeln!(self.writer)?;
    write!(self.writer, "  | ")?;
    for _ in 1..ctx.col_num {
      write!(self.writer, " ")?;
    }
    writeln!(self.writer, "^")?;
    writeln!(self.writer)?;
    Ok(())
  }

  pub fn report_io(
    &mut self,
    name: &str,
    err: std::io::Error,
  ) -> std::io::Result<()> {
    writeln!(self.writer, "error: {}", err)?;
    writeln!(self.writer, " --> {}", name)?;
    writeln!(self.writer)?;
    Ok(())
  }
}
