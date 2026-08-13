//! [`SCon`], a special value constructor.

use std::fmt;
use str_util::SmolStr;

/// A special constructor, also called a "literal".
#[derive(Debug, Clone)]
pub enum SCon {
  /// An int literal.
  Int(Int),
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

/// An int.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int(i64);

impl fmt::Display for Int {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl From<i32> for Int {
  fn from(value: i32) -> Self {
    Self(value.into())
  }
}

impl std::ops::Add for Int {
  type Output = Int;

  fn add(self, rhs: Self) -> Self::Output {
    Self(self.0 + rhs.0)
  }
}

impl std::ops::Mul<i32> for Int {
  type Output = Int;

  fn mul(self, rhs: i32) -> Self::Output {
    Self(self.0 * i64::from(rhs))
  }
}

impl Int {
  /// Parses an int with the given radix from a string.
  ///
  /// # Errors
  ///
  /// When parsing failed.
  ///
  /// # Panics
  ///
  /// When the radix was invalid.
  pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, ParseIntError> {
    match i64::from_str_radix(s, radix) {
      Ok(x) => Ok(Int(x)),
      Err(e) => Err(ParseIntError(e)),
    }
  }
}

/// An error when parsing an [`Int`] from a str.
#[derive(Debug)]
pub struct ParseIntError(std::num::ParseIntError);

impl fmt::Display for ParseIntError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}
