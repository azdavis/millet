use crate::parser::{Exited, Parser};
use crate::util::many_sep;
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_>) -> Exited {
  todo!()
}

pub(crate) fn ty_var_seq(p: &mut Parser<'_>) -> Exited {
  let ent = p.enter();
  if p.at(SK::TyVar) {
    let ent = p.enter();
    p.bump();
    p.exit(ent, SK::TyVarArg);
  } else if p.at(SK::LRound) && p.peek_n(1).map_or(false, |tok| tok.kind == SK::TyVar) {
    p.bump();
    many_sep(p, SK::Comma, SK::TyVarArg, |p| {
      p.eat(SK::TyVar);
    });
    p.eat(SK::RRound);
  }
  p.exit(ent, SK::TyVarSeq)
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
