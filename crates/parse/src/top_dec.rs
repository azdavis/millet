use crate::dec::{dat_binds, datatype_copy, dec_one};
use crate::parser::{Exited, Expected, Parser};
use crate::ty::{of_ty, ty, ty_var_seq};
use crate::util::{eat_name_star, many_sep, maybe_semi_sep, must, path};
use syntax::SyntaxKind as SK;

pub(crate) fn str_dec(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  let ret = maybe_semi_sep(p, SK::StrDecInSeq, str_dec_one);
  p.exit(en, SK::StrDec);
  ret
}

#[must_use]
fn str_dec_one(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  if p.at(SK::FunctorKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::FunctorBind, |p| {
      p.eat(SK::Name);
      p.eat(SK::LRound);
      if p.at(SK::Name) {
        let en = p.enter();
        p.bump();
        p.eat(SK::Colon);
        must(p, sig_exp, Expected::SigExp);
        p.exit(en, SK::FunctorArgNameSigExp);
      } else {
        spec(p);
      }
      p.eat(SK::RRound);
      if ascription(p) {
        ascription_tail(p);
      }
      p.eat(SK::Eq);
      must(p, str_exp, Expected::StrExp);
    });
    p.exit(en, SK::FunctorDec);
  } else if p.at(SK::SignatureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::SigBind, |p| {
      p.eat(SK::Name);
      p.eat(SK::Eq);
      must(p, sig_exp, Expected::SigExp);
    });
    p.exit(en, SK::SigDec);
  } else if p.at(SK::StructureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::StrBind, |p| {
      p.eat(SK::Name);
      if ascription(p) {
        ascription_tail(p);
      }
      p.eat(SK::Eq);
      must(p, str_exp, Expected::StrExp);
    });
    p.exit(en, SK::StructureStrDec);
  } else if p.at(SK::LocalKw) {
    // LocalStrDec is a 'superset' of LocalDec, so always use the former
    p.bump();
    str_dec(p);
    p.eat(SK::InKw);
    str_dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::LocalStrDec);
  } else if dec_one(p) {
    p.exit(en, SK::DecStrDec);
  } else {
    p.abandon(en);
    return false;
  }
  true
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
    let arg = p.enter();
    if str_exp(p).is_some() {
      p.exit(arg, SK::AppStrExpArgStrExp);
    } else {
      p.abandon(arg);
      str_dec(p);
    }
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
  // the first 'where' must actually be 'where', but further ones can be 'and'.
  if !p.at(SK::WhereKw) {
    return Some(ex);
  }
  while p.at(SK::WhereKw) || p.at(SK::AndKw) {
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
fn spec_one(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  let mut ex = if p.at(SK::ValKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ValDesc, |p| {
      eat_name_star(p);
      p.eat(SK::Colon);
      ty(p);
    });
    p.exit(en, SK::ValSpec)
  } else if p.at(SK::TypeKw) {
    ty_spec(p);
    p.exit(en, SK::TySpec)
  } else if p.at(SK::EqtypeKw) {
    ty_spec(p);
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
      eat_name_star(p);
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
    return false;
  };
  while p.at(SK::SharingKw) {
    let en = p.precede(ex);
    p.bump();
    if p.at(SK::TypeKw) {
      p.bump();
    }
    many_sep(p, SK::Eq, SK::PathEq, |p| {
      must(p, path, Expected::Path);
    });
    ex = p.exit(en, SK::SharingSpec);
  }
  true
}

fn ty_spec(p: &mut Parser<'_>) {
  p.bump();
  many_sep(p, SK::AndKw, SK::TyDesc, |p| {
    ty_var_seq(p);
    p.eat(SK::Name);
    if p.at(SK::Eq) {
      let en = p.enter();
      p.bump();
      ty(p);
      p.exit(en, SK::EqTy);
    }
  });
}

fn spec(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  maybe_semi_sep(p, SK::SpecInSeq, spec_one);
  p.exit(en, SK::Spec)
}
