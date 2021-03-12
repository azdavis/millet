use crate::parser::Parser;
use crate::top_dec::top_dec;
use crate::util::OpCx;
use syntax::SyntaxKind as SK;

pub(crate) fn root(p: &mut Parser<'_>) {
  let entered = p.enter();
  let mut cx = OpCx::default();
  while p.peek().is_some() {
    top_dec(p, &mut cx);
  }
  p.exit(entered, SK::Root);
}
