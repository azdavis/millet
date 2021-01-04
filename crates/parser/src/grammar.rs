mod expr;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use lexer::TokenKind;
use syntax::SyntaxKind;

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
  let m = p.start();
  expr::expr(p);
  m.complete(p, SyntaxKind::ROOT)
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct OpInfo(u8, u8);

impl OpInfo {
  pub(crate) fn right(level: u8) -> OpInfo {
    OpInfo(level * 2 + 1, level * 2)
  }

  pub(crate) fn left(level: u8) -> OpInfo {
    OpInfo(level * 2, level * 2 + 1)
  }
}
