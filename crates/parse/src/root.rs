use crate::parser::{ErrorKind, Expected, Parser};
use crate::top_dec::top_dec;
use crate::util::maybe_semi_sep;
use syntax::SyntaxKind as SK;

pub(crate) fn root(p: &mut Parser<'_>) {
  let entered = p.enter();
  while p.peek().is_some() {
    if !maybe_semi_sep(p, SK::TopDecInSeq, top_dec) {
      // avoid infinite loop
      p.error(ErrorKind::Expected(Expected::Item));
      p.bump();
    }
  }
  p.exit(entered, SK::Root);
}
