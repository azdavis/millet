use syntax::event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_, SK>) -> Exited {
  todo!()
}

pub(crate) fn ty_var_seq(p: &mut Parser<'_, SK>) -> Exited {
  todo!()
}

#[must_use]
pub(crate) fn of_ty(p: &mut Parser<'_, SK>) -> Option<Exited> {
  tok_ty(p, SK::OfKw, SK::OfTy)
}

#[must_use]
pub(crate) fn ty_annotation(p: &mut Parser<'_, SK>) -> Option<Exited> {
  tok_ty(p, SK::Colon, SK::TyAnnotation)
}

#[must_use]
fn tok_ty(p: &mut Parser<'_, SK>, tok: SK, wrap: SK) -> Option<Exited> {
  if p.at(tok) {
    let ent = p.enter();
    p.bump();
    ty(p);
    Some(p.exit(ent, wrap))
  } else {
    None
  }
}
