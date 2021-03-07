//! Parses tokens into a concrete syntax tree.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use std::fmt;
use syntax::ast::{Cast as _, Root};
use syntax::event_parse::{Parser, Sink, Token};
use syntax::rowan::{GreenNodeBuilder, TextRange, TextSize};
use syntax::{SyntaxKind as SK, SyntaxNode};

mod dec;
mod exp;
mod pat;
mod root;
mod top_dec;
mod ty;
mod util;

/// The result of a parse.
#[derive(Debug)]
pub struct Parse {
  /// The root.
  pub root: Root,
  /// The errors encountered when parsing.
  pub errors: Vec<Error>,
}

/// A parse error.
#[derive(Debug)]
pub struct Error {
  /// The range of the unexpected token.
  pub range: TextRange,
  /// The tokens that would have been allowed.
  pub expected: Expected,
}

/// A list of expected tokens.
#[derive(Debug)]
pub struct Expected {
  /// The token kinds.
  pub kinds: Vec<SK>,
}

impl fmt::Display for Expected {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.kinds.iter();
    write!(f, "expected any of ")?;
    if let Some(kind) = iter.next() {
      write!(f, "{}", kind.token_desc().unwrap_or("<non-token>"))?;
    }
    for kind in iter {
      write!(f, ", {}", kind.token_desc().unwrap_or("<non-token>"))?;
    }
    Ok(())
  }
}

/// Returns a parse of the tokens.
pub fn get(tokens: Vec<Token<'_, SK>>) -> Parse {
  let mut p = Parser::new(tokens);
  root::root(&mut p);
  let mut sink = BuilderSink::default();
  p.finish(&mut sink);
  let node = SyntaxNode::new_root(sink.builder.finish());
  Parse {
    root: Root::cast(node.into()).unwrap(),
    errors: sink.errors,
  }
}

#[derive(Default)]
struct BuilderSink {
  builder: GreenNodeBuilder<'static>,
  range: Option<TextRange>,
  errors: Vec<Error>,
}

impl Sink<SK> for BuilderSink {
  fn enter(&mut self, kind: SK) {
    self.builder.start_node(kind.into());
  }

  fn token(&mut self, token: Token<'_, SK>) {
    self.builder.token(token.kind.into(), token.text);
    let start = self.range.as_ref().map_or(0.into(), |range| range.end());
    let end = start + TextSize::of(token.text);
    self.range = Some(TextRange::new(start, end));
  }

  fn exit(&mut self) {
    self.builder.finish_node();
  }

  fn error(&mut self, kinds: Vec<SK>) {
    self.errors.push(Error {
      range: self.range.clone().expect("error with no tokens"),
      expected: Expected { kinds },
    });
  }
}
