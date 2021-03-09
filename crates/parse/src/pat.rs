use crate::util::OpCx;
use syntax::event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

#[must_use]
pub(crate) fn pat<'a>(p: &mut Parser<'a, SK>, cx: &OpCx<'a>) -> Option<Exited> {
  todo!()
}
