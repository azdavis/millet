use crate::parser::{Exited, Expected, Parser};
use crate::util::{comma_sep, lab, must, path};
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_>) {
  must(p, |p| ty_prec(p, TyPrec::Arrow), Expected::Ty)
}

#[must_use]
fn ty_prec(p: &mut Parser<'_>, min_prec: TyPrec) -> Option<Exited> {
  let en = p.enter();
  let mut ex = if p.at(SK::TyVar) {
    p.bump();
    p.exit(en, SK::TyVarTy)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::TyRow, |p| {
      must(p, lab, Expected::Lab);
      p.eat(SK::Colon);
      ty(p);
    });
    p.exit(en, SK::RecordTy)
  } else if p.at(SK::LRound) {
    let save = p.save();
    p.bump();
    ty(p);
    p.eat(SK::RRound);
    if p.maybe_discard(save) {
      p.exit(en, SK::ParenTy)
    } else {
      let ty_seq = p.enter();
      p.bump();
      comma_sep(p, SK::RRound, SK::TyArg, ty);
      p.exit(ty_seq, SK::TySeq);
      must(p, path, Expected::Path);
      p.exit(en, SK::ConTy)
    }
  } else if p.at(SK::Name) {
    must(p, path, Expected::Path);
    p.exit(en, SK::ConTy)
  } else {
    p.abandon(en);
    return None;
  };
  loop {
    ex = if p.at(SK::MinusGt) {
      if TyPrec::Arrow < min_prec {
        break;
      }
      let en = p.precede(ex);
      p.bump();
      must(p, |p| ty_prec(p, TyPrec::Arrow), Expected::Ty);
      p.exit(en, SK::FnTy)
    } else if p.at(SK::Star) {
      if TyPrec::Star < min_prec {
        break;
      }
      let en = p.precede(ex);
      while p.at(SK::Star) {
        p.bump();
        must(p, |p| ty_prec(p, TyPrec::App), Expected::Ty);
      }
      p.exit(en, SK::TupleTy)
    } else if p.at(SK::Name) {
      let en = p.precede(ex);
      must(p, path, Expected::Path);
      p.exit(en, SK::ConTy)
    } else {
      break;
    };
  }
  Some(ex)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

pub(crate) fn ty_var_seq(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  if p.at(SK::TyVar) {
    let en = p.enter();
    p.bump();
    p.exit(en, SK::TyVarArg);
  } else if p.at(SK::LRound) && p.at_n(1, SK::TyVar) {
    p.bump();
    comma_sep(p, SK::RRound, SK::TyVarArg, |p| {
      p.eat(SK::TyVar);
    });
  }
  p.exit(en, SK::TyVarSeq)
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
    let en = p.enter();
    p.bump();
    ty(p);
    Some(p.exit(en, wrap))
  } else {
    None
  }
}
