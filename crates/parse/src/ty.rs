use crate::parser::{Exited, Expected, Parser};
use crate::util::{comma_sep, lab, must, path};
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_>) {
  must(p, |p| ty_prec(p, TyPrec::Arrow), Expected::Ty)
}

fn ty_prec(p: &mut Parser<'_>, min_prec: TyPrec) -> Option<Exited> {
  let en = p.enter();
  let mut ex = if p.at(SK::Underscore) {
    p.bump();
    p.exit(en, SK::HoleTy)
  } else if p.at(SK::TyVar) {
    p.bump();
    p.exit(en, SK::TyVarTy)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::TyRow, |p| {
      lab(p);
      p.eat(SK::Colon);
      ty(p);
    });
    p.exit(en, SK::RecordTy)
  } else if p.at(SK::LRound) {
    let ty_seq = p.enter();
    p.bump();
    let ty_arg = p.enter();
    ty(p);
    if p.at(SK::RRound) {
      p.abandon(ty_arg);
      p.abandon(ty_seq);
      p.bump();
      p.exit(en, SK::ParenTy)
    } else {
      p.eat(SK::Comma);
      p.exit(ty_arg, SK::TyArg);
      loop {
        let en = p.enter();
        ty(p);
        if p.at(SK::Comma) {
          p.bump();
          p.exit(en, SK::TyArg);
        } else {
          p.exit(en, SK::TyArg);
          p.eat(SK::RRound);
          break;
        }
      }
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
        let en = p.enter();
        p.bump();
        must(p, |p| ty_prec(p, TyPrec::App), Expected::Ty);
        p.exit(en, SK::StarTy);
      }
      p.exit(en, SK::TupleTy)
    } else if p.at(SK::Name) {
      let en = p.precede(ex);
      must(p, path, Expected::Path);
      p.exit(en, SK::OneArgConTy)
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

pub(crate) fn of_ty(p: &mut Parser<'_>) -> Option<Exited> {
  tok_ty(p, SK::OfKw, SK::OfTy)
}

pub(crate) fn ty_annotation(p: &mut Parser<'_>) -> Option<Exited> {
  tok_ty(p, SK::Colon, SK::TyAnnotation)
}

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
