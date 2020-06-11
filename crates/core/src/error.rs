//! Errors.

use crate::lex::LexError;
use crate::parse::ParseError;
use std::fmt;

#[derive(Debug)]
pub enum Error {
  Lex(LexError),
  Parse(ParseError),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Lex(e) => e.fmt(f),
      Self::Parse(e) => e.fmt(f),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::Lex(e) => Some(e),
      Self::Parse(e) => Some(e),
    }
  }
}
