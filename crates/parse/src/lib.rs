//! Parses tokens into a concrete syntax tree.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod dec;
mod exp;
mod parser;
mod pat;
mod root;
mod top_dec;
mod ty;
mod util;

use crate::parser::{Error, Parser};
use syntax::ast::{Cast as _, Root};
use syntax::{token::Token, SyntaxKind as SK};

/// The result of a parse.
#[derive(Debug)]
pub struct Parse {
  /// The root.
  pub root: Root,
  /// The errors encountered when parsing.
  pub errors: Vec<Error>,
}

/// Returns a parse of the tokens.
pub fn get(tokens: &[Token<'_, SK>]) -> Parse {
  let mut p = Parser::new(tokens);
  root::root(&mut p);
  let (node, errors) = p.finish();
  Parse {
    root: Root::cast(node.into()).unwrap(),
    errors,
  }
}
