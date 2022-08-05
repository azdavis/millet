//! Utilities for formatting.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::fmt;

/// Format the iter, interspersed with `sep`.
pub fn sep_seq<I, T>(f: &mut fmt::Formatter<'_>, sep: &str, mut iter: I) -> fmt::Result
where
  I: Iterator<Item = T>,
  T: fmt::Display,
{
  if let Some(x) = iter.next() {
    x.fmt(f)?;
  }
  for x in iter {
    f.write_str(sep)?;
    x.fmt(f)?;
  }
  Ok(())
}

/// `seq_seq` with `", "` as the separator.
pub fn comma_seq<I, T>(f: &mut fmt::Formatter<'_>, iter: I) -> fmt::Result
where
  I: Iterator<Item = T>,
  T: fmt::Display,
{
  sep_seq(f, ", ", iter)
}
