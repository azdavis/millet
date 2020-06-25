//! Conversion from library error types to codespan Diagnostics.

use crate::source::SourceId;
use codespan_reporting::diagnostic::Label;
use millet_core::intern::StrStore;
use millet_core::lex::LexError;
use millet_core::loc::Located;
use millet_core::parse::ParseError;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<SourceId>;

pub fn io(name: &str, err: std::io::Error) -> Diagnostic {
  Diagnostic::error().with_message(format!("{}: {}", name, err))
}

pub fn lex(id: SourceId, err: Located<LexError>) -> Diagnostic {
  let msg = match err.val {
    LexError::UnmatchedCloseComment => "unmatched close comment".to_owned(),
    LexError::UnmatchedOpenComment => "unmatched open comment".to_owned(),
    LexError::IncompleteTypeVar => "incomplete type var".to_owned(),
    LexError::UnknownByte(b) => format!("unknown byte: {}", b),
    LexError::InvalidIntConstant(e) => format!("invalid integer constant: {}", e),
    LexError::InvalidRealConstant(e) => format!("invalid real constant: {}", e),
    LexError::NegativeWordConstant => "negative word constant".to_owned(),
    LexError::IncompleteNumConstant => "incomplete numeric constant".to_owned(),
    LexError::UnclosedStringConstant => "unclosed string constant".to_owned(),
    LexError::InvalidStringConstant => "invalid string constant".to_owned(),
    LexError::InvalidCharConstant => "invalid character constant".to_owned(),
  };
  Diagnostic::error()
    .with_message(msg)
    .with_labels(vec![Label::primary(id, err.loc)])
}

pub fn parse(store: &StrStore, id: SourceId, err: Located<ParseError>) -> Diagnostic {
  let msg = match err.val {
    ParseError::ExpectedButFound(exp, fnd) => format!("expected {}, found {}", exp, fnd),
    ParseError::InfixWithoutOp(id) => format!(
      "infix identifier used without preceding `op`: {}",
      store.get(id)
    ),
    ParseError::NotInfix(id) => format!("non-infix identifier used as infix: {}", store.get(id)),
    ParseError::RealPat => "real constant used as a pattern".to_owned(),
    ParseError::NegativeFixity(n) => format!("fixity is negative: {}", n),
  };
  Diagnostic::error()
    .with_message(msg)
    .with_labels(vec![Label::primary(id, err.loc)])
}
