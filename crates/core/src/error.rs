//! Errors.

use crate::lex::LexError;
use crate::loc::Located;
use crate::parse::ParseError;

#[derive(Debug)]
pub enum Error {
  Lex(Located<LexError>),
  Parse(Located<ParseError>),
}
