//! [`SCon`], a special value constructor.

use std::fmt;
use str_util::SmolStr;

/// A special constructor, also called a "literal".
#[derive(Debug, Clone)]
pub enum SCon {
  /// An int literal.
  Int(i64),
  /// A real literal.
  Real(f64),
  /// A word literal.
  Word(u64),
  /// A char literal.
  Char(char),
  /// A string literal.
  String(SmolStr),
}

impl fmt::Display for SCon {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      // TODO doesn't preserve hex/not hex info
      SCon::Int(int) => int.fmt(f),
      // TODO doesn't really show exp
      SCon::Real(r) => r.fmt(f),
      // TODO doesn't preserve hex/not hex info
      SCon::Word(w) => write!(f, "0w{w}"),
      // TODO not totally accurate with escapes
      SCon::Char(c) => write!(f, "#\"{c}\""),
      // TODO not totally accurate with escapes
      SCon::String(s) => write!(f, "\"{s}\""),
    }
  }
}
