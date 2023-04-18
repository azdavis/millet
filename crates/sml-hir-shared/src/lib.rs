//! Some shared types.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use num_traits::Num as _;
use std::fmt;
use str_util::{Name, SmolStr};

/// A record/tuple label.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lab {
  /// A named label.
  Name(Name),
  /// A numeric label.
  Num(usize),
}

impl fmt::Display for Lab {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Name(name) => name.fmt(f),
      Self::Num(n) => n.fmt(f),
    }
  }
}

impl Lab {
  /// Return the numeric label for one greater than the number passed.
  #[must_use]
  pub fn tuple(idx: usize) -> Self {
    Self::Num(idx + 1)
  }
}

/// A special constructor, aka a literal.
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

/// An int.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int(IntRepr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum IntRepr {
  Finite(i32),
  Big(num_bigint::BigInt),
}

impl fmt::Display for Int {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.0 {
      IntRepr::Finite(x) => x.fmt(f),
      IntRepr::Big(x) => x.fmt(f),
    }
  }
}

impl From<i32> for Int {
  fn from(value: i32) -> Self {
    Self(IntRepr::Finite(value))
  }
}

impl std::ops::Mul<i32> for Int {
  type Output = Int;

  fn mul(self, rhs: i32) -> Self::Output {
    let repr = match self.0 {
      IntRepr::Finite(x) => IntRepr::Finite(x * rhs),
      IntRepr::Big(x) => IntRepr::Big(x * rhs),
    };
    Self(repr)
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
    match i32::from_str_radix(s, radix) {
      Ok(x) => Ok(Int(IntRepr::Finite(x))),
      Err(_) => match num_bigint::BigInt::from_str_radix(s, radix) {
        Ok(x) => Ok(Int(IntRepr::Big(x))),
        Err(e) => Err(ParseIntError(e)),
      },
    }
  }
}

/// An error when parsing an [`Int`] from a str.
#[derive(Debug)]
pub struct ParseIntError(num_bigint::ParseBigIntError);

impl fmt::Display for ParseIntError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}
