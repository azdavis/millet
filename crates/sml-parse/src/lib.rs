//! Parsing tokens into a concrete syntax tree.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

mod dec;
mod exp;
mod pat;
mod root;
mod ty;
mod util;

pub mod parser;

use sml_syntax::ast::{AstNode as _, Root};
use sml_syntax::{token::Token, SyntaxKind as SK};

/// The result of a parse.
#[derive(Debug)]
pub struct Parse {
  /// The root.
  pub root: Root,
  /// The errors encountered when parsing.
  pub errors: Vec<parser::Error>,
}

/// Returns a parse of the tokens.
///
/// # Panics
///
/// If casting the root node to a Root failed (an internal error).
pub fn get(tokens: &[Token<'_, SK>], fe: &mut sml_fixity::Env) -> Parse {
  let mut p = parser::Parser::new(tokens);
  root::root(&mut p, fe);
  let mut sink = event_parse::rowan_sink::RowanSink::default();
  p.finish(&mut sink);
  let (node, errors) = sink.finish();
  Parse { root: Root::cast(node).unwrap(), errors: errors.into_iter().map(parser::Error).collect() }
}
