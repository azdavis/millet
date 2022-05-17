use crate::parser::Parser;
use crate::top_dec::top_dec;
use syntax::SyntaxKind as SK;

pub(crate) fn root(p: &mut Parser<'_>) {
  let entered = p.enter();
  while p.peek().is_some() {
    if !top_dec(p) {
      // avoid infinite loop
      p.bump();
    }
  }
  p.exit(entered, SK::Root);
}
