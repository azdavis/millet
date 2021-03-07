use crate::dec::dec;
use crate::ty::{of_ty, ty, ty_var_seq};
use crate::util::{many_sep, maybe_semi_sep, must, path};
use syntax::event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn top_dec(p: &mut Parser<'_, SK>) -> Exited {
  let ent = p.enter();
  if p.at(SK::FunctorKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::FunctorBind, |p| {
      p.eat(SK::Name);
      p.eat(SK::LRound);
      p.eat(SK::Name);
      p.eat(SK::Colon);
      must(p, sig_exp);
      p.eat(SK::RRound);
      p.eat(SK::Eq);
      str_exp(p);
    });
    p.exit(ent, SK::FunctorDec)
  } else if p.at(SK::SignatureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::SigBind, |p| {
      p.eat(SK::Name);
      p.eat(SK::Eq);
      must(p, sig_exp);
    });
    p.exit(ent, SK::SigDec)
  } else {
    str_dec(p)
  }
}

fn str_dec(p: &mut Parser<'_, SK>) -> Exited {
  let ent = p.enter();
  if p.at(SK::StructureKw) {
    p.bump();
    loop {
      p.eat(SK::Name);
      p.eat(SK::Eq);
      str_exp(p);
      if p.at(SK::AndKw) {
        p.bump();
      } else {
        break;
      }
    }
    p.exit(ent, SK::StructureStrDec)
  } else if p.at(SK::LocalKw) {
    // LocalStrDec is a 'superset' of LocalDec, so always use the former
    p.bump();
    str_dec(p);
    p.eat(SK::InKw);
    str_dec(p);
    p.eat(SK::EndKw);
    p.exit(ent, SK::LocalStrDec)
  } else {
    dec(p)
  }
}

fn str_exp(p: &mut Parser<'_, SK>) -> Exited {
  let ent = p.enter();
  let mut ex = if p.at(SK::StructKw) {
    p.bump();
    str_dec(p);
    p.eat(SK::EndKw);
    p.exit(ent, SK::StructStrExp)
  } else if p.at(SK::LetKw) {
    p.bump();
    str_dec(p);
    p.eat(SK::InKw);
    str_exp(p);
    p.eat(SK::EndKw);
    p.exit(ent, SK::LetStrExp)
  } else if p.at(SK::Name) && p.peek_n(1).map_or(false, |x| x.kind == SK::Dot) {
    path(p);
    p.exit(ent, SK::PathStrExp)
  } else {
    p.eat(SK::Name);
    p.eat(SK::LRound);
    str_exp(p);
    p.eat(SK::RRound);
    p.exit(ent, SK::AppStrExp)
  };
  while p.at(SK::Colon) || p.at(SK::ColonGt) {
    let ent = p.precede(ex);
    p.bump();
    must(p, sig_exp);
    ex = p.exit(ent, SK::AscriptionStrExp);
  }
  ex
}

#[must_use]
fn sig_exp(p: &mut Parser<'_, SK>) -> Option<Exited> {
  let ent = p.enter();
  let mut ex = if p.at(SK::SigKw) {
    p.bump();
    spec(p);
    p.eat(SK::EndKw);
    p.exit(ent, SK::SigSigExp)
  } else if p.at(SK::Name) {
    p.bump();
    p.exit(ent, SK::NameSigExp)
  } else {
    p.abandon(ent);
    return None;
  };
  while p.at(SK::WhereKw) {
    let ent = p.precede(ex);
    p.bump();
    p.eat(SK::TypeKw);
    ty_var_seq(p);
    path(p);
    p.eat(SK::Eq);
    ty(p);
    ex = p.exit(ent, SK::WhereSigExp);
  }
  Some(ex)
}

#[must_use]
fn spec_one(p: &mut Parser<'_, SK>) -> Option<Exited> {
  let ent = p.enter();
  let mut ex = if p.at(SK::ValKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ValDesc, |p| {
      p.eat(SK::Name);
      p.eat(SK::Colon);
      ty(p);
    });
    p.exit(ent, SK::ValSpec)
  } else if p.at(SK::TypeKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::TyDesc, |p| {
      ty_var_seq(p);
      p.eat(SK::Name);
    });
    p.exit(ent, SK::TySpec)
  } else if p.at(SK::EqtypeKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::TyDesc, |p| {
      ty_var_seq(p);
      p.eat(SK::Name);
    });
    p.exit(ent, SK::EqTySpec)
  } else if p.at(SK::DatatypeKw) {
    // TODO datatype copy spec
    p.bump();
    many_sep(p, SK::AndKw, SK::DatDesc, |p| {
      ty_var_seq(p);
      p.eat(SK::Name);
      p.eat(SK::Eq);
      many_sep(p, SK::Bar, SK::ConDesc, |p| {
        p.eat(SK::Name);
        of_ty(p);
      });
    });
    p.exit(ent, SK::DatatypeSpec)
  } else if p.at(SK::ExceptionKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ExDesc, |p| {
      p.eat(SK::Name);
      of_ty(p);
    });
    p.exit(ent, SK::ExSpec)
  } else if p.at(SK::StructureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::StrDesc, |p| {
      p.eat(SK::Name);
      p.eat(SK::Colon);
      must(p, sig_exp);
    });
    p.exit(ent, SK::StrSpec)
  } else if p.at(SK::IncludeKw) {
    p.bump();
    while sig_exp(p).is_some() {
      // no body
    }
    p.exit(ent, SK::IncludeSpec)
  } else {
    p.abandon(ent);
    return None;
  };
  while p.at(SK::SharingKw) {
    let ent = p.precede(ex);
    p.bump();
    p.eat(SK::TypeKw);
    many_sep(p, SK::Eq, SK::PathEq, |p| {
      path(p);
    });
    ex = p.exit(ent, SK::SharingSpec);
  }
  Some(ex)
}

fn spec(p: &mut Parser<'_, SK>) -> Exited {
  let ent = p.enter();
  maybe_semi_sep(p, SK::SpecInSeq, spec_one);
  p.exit(ent, SK::Spec)
}
