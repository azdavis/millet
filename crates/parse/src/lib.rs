//! Parses tokens into a concrete syntax tree.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod dec;
mod exp;
mod pat;
mod root;
mod top_dec;
mod ty;
mod util;

pub mod parser;

use syntax::ast::AstNode as _;
use syntax::{token::Token, SyntaxKind as SK};

pub use syntax::ast::Root;

/// The result of a parse.
#[derive(Debug)]
pub struct Parse {
  /// The root.
  pub root: Root,
  /// The errors encountered when parsing.
  pub errors: Vec<parser::Error>,
}

/// Returns a parse of the tokens.
pub fn get<'a>(tokens: &'a [Token<'a, SK>], infix: &'a mut parser::FixEnv<'a>) -> Parse {
  let mut p = parser::Parser::new(tokens, infix);
  root::root(&mut p);
  let (node, errors) = p.finish();
  Parse {
    root: Root::cast(node).unwrap(),
    errors,
  }
}
