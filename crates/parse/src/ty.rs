use crate::parser::{Exited, Expected, Parser};
use crate::util::{comma_sep, lab, must, path};
use syntax::SyntaxKind as SK;

const STAR: &str = "*";

pub(crate) fn ty(p: &mut Parser<'_>) {
  must(p, |p| ty_prec(p, TyPrec::Arrow), Expected::Ty)
}

#[must_use]
fn ty_prec(p: &mut Parser<'_>, min_prec: TyPrec) -> Option<Exited> {
  let ent = p.enter();
  let mut ex = if p.at(SK::TyVar) {
    p.bump();
    p.exit(ent, SK::TyVarTy)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::TyRow, |p| {
      must(p, lab, Expected::Lab);
      p.eat(SK::Colon);
      ty(p);
    });
    p.exit(ent, SK::RecordTy)
  } else if p.at(SK::LRound) {
    let save = p.save();
    p.bump();
    ty(p);
    p.eat(SK::RRound);
    if p.maybe_discard(save) {
      p.exit(ent, SK::ParenTy)
    } else {
      let ty_seq = p.enter();
      p.bump();
      comma_sep(p, SK::RRound, SK::TyArg, ty);
      p.exit(ty_seq, SK::TySeq);
      must(p, path, Expected::Path);
      p.exit(ent, SK::ConTy)
    }
  } else if p
    .peek()
    .map_or(false, |tok| tok.kind == SK::Name && tok.text != STAR)
  {
    must(p, path, Expected::Path);
    p.exit(ent, SK::ConTy)
  } else {
    p.abandon(ent);
    return None;
  };
  loop {
    ex = if p.at(SK::MinusGt) {
      if TyPrec::Arrow < min_prec {
        break;
      }
      let ent = p.precede(ex);
      p.bump();
      must(p, |p| ty_prec(p, TyPrec::Arrow), Expected::Ty);
      p.exit(ent, SK::FnTy)
    } else if p.at(SK::Name) {
      if p.peek().unwrap().text == STAR {
        if TyPrec::Star < min_prec {
          break;
        }
        let ent = p.precede(ex);
        while p
          .peek()
          .map_or(false, |tok| tok.kind == SK::Name && tok.text == STAR)
        {
          p.bump();
          must(p, |p| ty_prec(p, TyPrec::App), Expected::Ty);
        }
        p.exit(ent, SK::TupleTy)
      } else {
        let ent = p.precede(ex);
        must(p, path, Expected::Path);
        p.exit(ent, SK::ConTy)
      }
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
  let ent = p.enter();
  if p.at(SK::TyVar) {
    let ent = p.enter();
    p.bump();
    p.exit(ent, SK::TyVarArg);
  } else if p.at(SK::LRound) && p.at_n(1, SK::TyVar) {
    p.bump();
    comma_sep(p, SK::RRound, SK::TyVarArg, |p| {
      p.eat(SK::TyVar);
    });
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
