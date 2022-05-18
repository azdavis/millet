use crate::dec::{dat_binds, datatype_copy, dec_one};
use crate::parser::{Entered, Exited, Expected, Parser};
use crate::ty::{of_ty, ty, ty_var_seq};
use crate::util::{many_sep, maybe_semi_sep, must, path};
use syntax::SyntaxKind as SK;

#[must_use]
/// returns whether this advanced.
pub(crate) fn top_dec(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  if p.at(SK::FunctorKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::FunctorBind, |p| {
      p.eat(SK::Name);
      p.eat(SK::LRound);
      p.eat(SK::Name);
      p.eat(SK::Colon);
      must(p, sig_exp, Expected::SigExp);
      p.eat(SK::RRound);
      if ascription(p) {
        ascription_tail(p);
      }
      p.eat(SK::Eq);
      must(p, str_exp, Expected::StrExp);
    });
    p.exit(en, SK::FunctorDec);
    true
  } else if p.at(SK::SignatureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::SigBind, |p| {
      p.eat(SK::Name);
      p.eat(SK::Eq);
      must(p, sig_exp, Expected::SigExp);
    });
    p.exit(en, SK::SigDec);
    true
  } else {
    str_dec_(p, en)
  }
}

fn str_dec(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  str_dec_(p, en)
}

fn str_dec_(p: &mut Parser<'_>, en: Entered) -> bool {
  let ret = maybe_semi_sep(p, SK::StrDecInSeq, str_dec_one);
  p.exit(en, SK::StrDec);
  ret
}

#[must_use]
fn str_dec_one(p: &mut Parser<'_>) -> Option<Exited> {
  let en = p.enter();
  let ex = if p.at(SK::StructureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::StrBind, |p| {
      p.eat(SK::Name);
      if ascription(p) {
        ascription_tail(p);
      }
      p.eat(SK::Eq);
      must(p, str_exp, Expected::StrExp);
    });
    p.exit(en, SK::StructureStrDec)
  } else if p.at(SK::LocalKw) {
    // LocalStrDec is a 'superset' of LocalDec, so always use the former
    p.bump();
    str_dec(p);
    p.eat(SK::InKw);
    str_dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::LocalStrDec)
  } else if dec_one(p).is_some() {
    p.exit(en, SK::DecStrDec)
  } else {
    p.abandon(en);
    return None;
  };
  Some(ex)
}

#[must_use]
fn str_exp(p: &mut Parser<'_>) -> Option<Exited> {
  let en = p.enter();
  let mut ex = if p.at(SK::StructKw) {
    p.bump();
    str_dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::StructStrExp)
  } else if p.at(SK::LetKw) {
    p.bump();
    str_dec(p);
    p.eat(SK::InKw);
    must(p, str_exp, Expected::StrExp);
    p.eat(SK::EndKw);
    p.exit(en, SK::LetStrExp)
  } else if p.at(SK::Name) && !p.at_n(1, SK::LRound) {
    must(p, path, Expected::Path);
    p.exit(en, SK::PathStrExp)
  } else if p.at(SK::Name) {
    p.bump();
    p.eat(SK::LRound);
    must(p, str_exp, Expected::StrExp);
    p.eat(SK::RRound);
    p.exit(en, SK::AppStrExp)
  } else {
    p.abandon(en);
    return None;
  };
  while ascription(p) {
    let en = p.precede(ex);
    ascription_tail(p);
    ex = p.exit(en, SK::AscriptionStrExp);
  }
  Some(ex)
}

fn ascription(p: &mut Parser<'_>) -> bool {
  p.at(SK::Colon) || p.at(SK::ColonGt)
}

/// should have just gotten `true` from [`ascription`]
fn ascription_tail(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  p.bump();
  must(p, sig_exp, Expected::SigExp);
  p.exit(en, SK::AscriptionTail)
}

#[must_use]
fn sig_exp(p: &mut Parser<'_>) -> Option<Exited> {
  let en = p.enter();
  let mut ex = if p.at(SK::SigKw) {
    p.bump();
    spec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::SigSigExp)
  } else if p.at(SK::Name) {
    p.bump();
    p.exit(en, SK::NameSigExp)
  } else {
    p.abandon(en);
    return None;
  };
  while p.at(SK::WhereKw) {
    let en = p.precede(ex);
    p.bump();
    p.eat(SK::TypeKw);
    ty_var_seq(p);
    must(p, path, Expected::Path);
    p.eat(SK::Eq);
    ty(p);
    ex = p.exit(en, SK::WhereSigExp);
  }
  Some(ex)
}

#[must_use]
fn spec_one(p: &mut Parser<'_>) -> Option<Exited> {
  let en = p.enter();
  let mut ex = if p.at(SK::ValKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ValDesc, |p| {
      p.eat(SK::Name);
      p.eat(SK::Colon);
      ty(p);
    });
    p.exit(en, SK::ValSpec)
  } else if p.at(SK::TypeKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::TyDesc, |p| {
      ty_var_seq(p);
      p.eat(SK::Name);
    });
    p.exit(en, SK::TySpec)
  } else if p.at(SK::EqtypeKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::TyDesc, |p| {
      ty_var_seq(p);
      p.eat(SK::Name);
    });
    p.exit(en, SK::EqTySpec)
  } else if p.at(SK::DatatypeKw) {
    if datatype_copy(p) {
      p.exit(en, SK::DatCopySpec)
    } else {
      dat_binds(p, false);
      p.exit(en, SK::DatSpec)
    }
  } else if p.at(SK::ExceptionKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ExDesc, |p| {
      p.eat(SK::Name);
      let _ = of_ty(p);
    });
    p.exit(en, SK::ExSpec)
  } else if p.at(SK::StructureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::StrDesc, |p| {
      p.eat(SK::Name);
      p.eat(SK::Colon);
      must(p, sig_exp, Expected::SigExp);
    });
    p.exit(en, SK::StrSpec)
  } else if p.at(SK::IncludeKw) {
    p.bump();
    while sig_exp(p).is_some() {
      // no body
    }
    p.exit(en, SK::IncludeSpec)
  } else {
    p.abandon(en);
    return None;
  };
  while p.at(SK::SharingKw) {
    let en = p.precede(ex);
    p.bump();
    p.eat(SK::TypeKw);
    many_sep(p, SK::Eq, SK::PathEq, |p| {
      must(p, path, Expected::Path);
    });
    ex = p.exit(en, SK::SharingSpec);
  }
  Some(ex)
}

fn spec(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  maybe_semi_sep(p, SK::SpecInSeq, spec_one);
  p.exit(en, SK::Spec)
}
