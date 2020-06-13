//! Conversion from library error types to codespan Diagnostics.

use crate::source::SourceFileId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use millet_core::error::Error;
use millet_core::intern::StrStore;
use millet_core::lex::LexError;
use millet_core::parse::ParseError;

pub fn new(store: &StrStore, file_id: SourceFileId, err: Error) -> Diagnostic<SourceFileId> {
  let (loc, message) = match err {
    Error::Lex(lex_err) => (
      lex_err.loc,
      match lex_err.val {
        LexError::UnmatchedCloseComment => format!("unmatched close comment"),
        LexError::UnmatchedOpenComment => format!("unmatched open comment"),
        LexError::IncompleteTypeVar => format!("incomplete type var"),
        LexError::UnknownByte(b) => format!("unknown byte: {}", b),
        LexError::InvalidIntConstant(e) => format!("invalid integer constant: {}", e),
        LexError::InvalidRealConstant(e) => format!("invalid real constant: {}", e),
        LexError::NegativeWordConstant => format!("negative word constant"),
        LexError::IncompleteNumConstant => format!("incomplete numeric constant"),
        LexError::UnclosedStringConstant => format!("unclosed string constant"),
        LexError::InvalidStringConstant => format!("invalid string constant"),
        LexError::InvalidCharConstant => format!("invalid character constant"),
      },
    ),
    Error::Parse(parse_err) => (
      parse_err.loc,
      match parse_err.val {
        ParseError::ExpectedButFound(exp, fnd) => format!("expected {}, found {}", exp, fnd),
        ParseError::InfixWithoutOp(id) => format!(
          "infix identifier `{}` used without preceding `op`",
          store.get(id)
        ),
        ParseError::NotInfix(id) => {
          format!("non-infix identifier `{}` used as infix", store.get(id))
        }
        ParseError::RealPat => format!("real constant used as a pattern"),
        ParseError::NegativeFixity(n) => format!("fixity `{}` is negative", n),
      },
    ),
  };
  Diagnostic::error()
    .with_message(message)
    .with_labels(vec![Label::primary(file_id, loc)])
}
