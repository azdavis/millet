use crate::top_dec::top_dec;
use syntax::event_parse::Parser;
use syntax::SyntaxKind as SK;

pub(crate) fn root(p: &mut Parser<'_, SK>) {
  let entered = p.enter();
  while p.peek().is_some() {
    top_dec(p);
  }
  p.exit(entered, SK::Root);
}
