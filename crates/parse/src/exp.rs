use crate::util::OpCx;
use syntax::event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn exp<'a>(p: &mut Parser<'a, SK>, cx: &OpCx<'a>) -> Exited {
  todo!()
}
