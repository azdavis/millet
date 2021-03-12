use crate::parser::{Exited, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_>) -> Exited {
  todo!()
}

pub(crate) fn ty_var_seq(p: &mut Parser<'_>) -> Exited {
  todo!()
}

#[must_use]
pub(crate) fn of_ty(p: &mut Parser<'_>) -> Option<Exited> {
  tok_ty(p, SK::OfKw, SK::OfTy)
}

#[must_use]
pub(crate) fn ty_annotation(p: &mut Parser<'_>) -> Option<Exited> {
  tok_ty(p, SK::Colon, SK::TyAnnotation)
}

#[must_use]
fn tok_ty(p: &mut Parser<'_>, tok: SK, wrap: SK) -> Option<Exited> {
  if p.at(tok) {
    let ent = p.enter();
    p.bump();
    ty(p);
    Some(p.exit(ent, wrap))
  } else {
    None
  }
}
