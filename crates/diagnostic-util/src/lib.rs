//! Utilities for diagnostics (colloquially, "errors") reported by Millet.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::fmt;

/// The url to go to for information about diagnostics.
pub const URL: &str = "https://github.com/azdavis/millet/blob/main/docs/diagnostics.md";

/// An error.
#[derive(Debug)]
pub struct Diagnostic {
  /// The range of the error.
  pub range: text_pos::Range,
  /// The message of the error.
  pub message: String,
  /// The error code.
  pub code: Code,
  /// The severity.
  pub severity: Severity,
}

/// The severity of this error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
  /// Warning. Should probably address.
  Warning,
  /// Error. The maximum. Pretty much means code cannot be run.
  Error,
}

/// An error code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Code(u16);

impl Code {
  /// Returns a Code for this.
  #[must_use]
  pub fn n(n: u16) -> Self {
    Self(n)
  }

  /// Return this as an [`i32`].
  #[must_use]
  pub fn as_i32(&self) -> i32 {
    self.0.into()
  }
}

impl fmt::Display for Code {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl std::str::FromStr for Code {
  type Err = ParseCodeError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match u16::from_str(s) {
      Ok(n) => Ok(Self(n)),
      Err(e) => Err(ParseCodeError(e)),
    }
  }
}

/// An error when a [`Code`] could not be parsed from a str.
#[derive(Debug)]
pub struct ParseCodeError(std::num::ParseIntError);

impl fmt::Display for ParseCodeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "couldn't parse code: {}", self.0)
  }
}

impl std::error::Error for ParseCodeError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    Some(&self.0)
  }
}
