//! Parsing a full program.

use crate::parser::{ErrorKind, Expected, Parser};
use sml_syntax::SyntaxKind as SK;

pub(crate) fn root(p: &mut Parser<'_>) {
  let entered = p.enter();
  while let Some(tok) = p.peek() {
    if !crate::dec::dec(p, crate::util::InfixErr::Yes) {
      // avoid infinite loop
      let ek = match tok.kind {
        SK::RRound | SK::RCurly | SK::RSquare | SK::EndKw => ErrorKind::UnmatchedClosingDelimiter,
        _ => ErrorKind::Expected(Expected::Item),
      };
      p.error(ek);
      p.bump();
    }
  }
  p.exit(entered, SK::Root);
}
