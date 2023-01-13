//! Parsing types.

use crate::parser::{ErrorKind, Exited, Expected, Parser};
use crate::util::{comma_sep, lab, path_must};
use sml_syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_>) {
  if ty_prec(p, TyPrec::Arrow).is_none() {
    p.error(ErrorKind::Expected(Expected::Ty));
  }
}

fn ty_prec(p: &mut Parser<'_>, min_prec: TyPrec) -> Option<Exited> {
  let en = p.enter();
  let mut ex = if p.at(SK::DotDotDot) {
    p.bump();
    p.exit(en, SK::HoleTy)
  } else if p.at(SK::Underscore) {
    p.bump();
    p.exit(en, SK::WildcardTy)
  } else if p.at(SK::TyVar) {
    p.bump();
    p.exit(en, SK::TyVarTy)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::TyRow, SK::RCurly, |p| {
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
      comma_sep(p, SK::TyArg, SK::RRound, ty);
      p.exit(ty_seq, SK::TySeq);
      path_must(p);
      p.exit(en, SK::ConTy)
    }
  } else if p.at(SK::Name) {
    path_must(p);
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
      if ty_prec(p, TyPrec::Arrow).is_none() {
        p.error(ErrorKind::Expected(Expected::Ty));
      }
      p.exit(en, SK::FnTy)
    } else if p.at(SK::Star) {
      if TyPrec::Star < min_prec {
        break;
      }
      let en = p.precede(ex);
      while p.at(SK::Star) {
        let en = p.enter();
        p.bump();
        if ty_prec(p, TyPrec::App).is_none() {
          p.error(ErrorKind::Expected(Expected::Ty));
        }
        p.exit(en, SK::StarTy);
      }
      p.exit(en, SK::TupleTy)
    } else if p.at(SK::Name) {
      let en = p.precede(ex);
      path_must(p);
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

/// returns if this consumed anything.
pub(crate) fn ty_var_seq(p: &mut Parser<'_>) -> bool {
  let mut ret = false;
  let en = p.enter();
  if p.at(SK::TyVar) {
    ret = true;
    let en = p.enter();
    p.bump();
    p.exit(en, SK::TyVarArg);
  } else if p.at(SK::LRound) && p.at_n(1, SK::TyVar) {
    ret = true;
    p.bump();
    comma_sep(p, SK::TyVarArg, SK::RRound, |p| {
      p.eat(SK::TyVar);
    });
  }
  if ret {
    p.exit(en, SK::TyVarSeq);
  } else {
    p.abandon(en);
  }
  ret
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
