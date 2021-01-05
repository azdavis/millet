mod dec;
mod expr;
mod ty;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use lexer::{IdentType, TokenKind};
use syntax::SyntaxKind;

const IDENT: [TokenKind; 2] = [
  TokenKind::IDENT(IdentType::Alphanumeric),
  TokenKind::IDENT(IdentType::Symbolic),
];

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
  let m = p.start();

  while !p.at_end() {
    dec::top_dec(p);
  }

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
