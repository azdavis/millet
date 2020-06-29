//! Conversion from library error types to codespan Diagnostics.

use crate::source::SourceId;
use codespan_reporting::diagnostic::Label;
use millet_core::intern::StrStore;
use millet_core::lex::LexError;
use millet_core::loc::{Loc, Located};
use millet_core::parse::ParseError;
use millet_core::statics::StaticsError;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<SourceId>;

pub fn io(name: &str, err: std::io::Error) -> Diagnostic {
  Diagnostic::error().with_message(format!("{}: {}", name, err))
}

pub fn lex(id: SourceId, err: Located<LexError>) -> Diagnostic {
  simple(err.val.show(), id, err.loc)
}

pub fn parse(store: &StrStore, id: SourceId, err: Located<ParseError>) -> Diagnostic {
  simple(err.val.show(store), id, err.loc)
}

pub fn statics(store: &StrStore, id: SourceId, err: Located<StaticsError>) -> Diagnostic {
  simple(err.val.show(store), id, err.loc)
}

fn simple<M: Into<String>>(msg: M, id: SourceId, loc: Loc) -> Diagnostic {
  Diagnostic::error()
    .with_message(msg)
    .with_labels(vec![Label::primary(id, loc)])
}
