use syntax::event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_, SK>) -> Exited {
  todo!()
}

pub(crate) fn ty_var_seq(p: &mut Parser<'_, SK>) -> Exited {
  todo!()
}

pub(crate) fn of_ty(p: &mut Parser<'_, SK>) -> Option<Exited> {
  if p.at(SK::OfKw) {
    p.bump();
    Some(ty(p))
  } else {
    None
  }
}
