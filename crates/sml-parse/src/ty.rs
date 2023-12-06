//! Parsing types.

use crate::parser::{ErrorKind, Exited, Expected, Parser};
use crate::util::{ascription, comma_sep, lab, path_must};
use sml_syntax::kind::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_>) {
  if ty_prec(p, TyPrec::Arrow).is_none() {
    p.error(ErrorKind::Expected(Expected::Ty));
  }
}

fn ty_prec(p: &mut Parser<'_>, min_prec: TyPrec) -> Option<Exited> {
  let kind = p.peek()?.kind;
  let en = p.enter();
  let kind = match kind {
    SK::DotDotDot => {
      p.bump();
      SK::HoleTy
    }
    SK::Underscore => {
      p.bump();
      SK::WildcardTy
    }
    SK::TyVar => {
      p.bump();
      SK::TyVarTy
    }
    SK::LCurly => {
      p.bump();
      comma_sep(p, SK::TyRow, SK::RCurly, |p| {
        lab(p);
        if ascription(p) {
          p.bump();
        }
        ty(p);
      });
      SK::RecordTy
    }
    SK::LRound => {
      let ty_seq = p.enter();
      p.bump();
      let ty_arg = p.enter();
      ty(p);
      if p.at(SK::RRound) {
        p.abandon(ty_arg);
        p.abandon(ty_seq);
        p.bump();
        SK::ParenTy
      } else {
        p.eat(SK::Comma);
        p.exit(ty_arg, SK::TyArg);
        comma_sep(p, SK::TyArg, SK::RRound, ty);
        p.exit(ty_seq, SK::TySeq);
        path_must(p);
        SK::ConTy
      }
    }
    SK::Name => {
      path_must(p);
      SK::ConTy
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  let mut ex = p.exit(en, kind);
  while let Some(tok) = p.peek() {
    ex = match tok.kind {
      SK::MinusGt => {
        if TyPrec::Arrow < min_prec {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        if ty_prec(p, TyPrec::Arrow).is_none() {
          p.error(ErrorKind::Expected(Expected::Ty));
        }
        p.exit(en, SK::FnTy)
      }
      SK::Star => {
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
      }
      SK::Name => {
        let en = p.precede(ex);
        path_must(p);
        p.exit(en, SK::OneArgConTy)
      }
      _ => break,
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
  if p.at(SK::OfKw) {
    let en = p.enter();
    p.bump();
    ty(p);
    Some(p.exit(en, SK::OfTy))
  } else {
    None
  }
}

pub(crate) fn ty_annotation(p: &mut Parser<'_>) -> Option<Exited> {
  if ascription(p) {
    let en = p.enter();
    p.bump();
    ty(p);
    Some(p.exit(en, SK::TyAnnotation))
  } else {
    None
  }
}
