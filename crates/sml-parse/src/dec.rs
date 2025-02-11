//! Parsing declarations.
//!
//! Note that we parse specifications as declarations and separate them out later.

use crate::exp::{eq_exp, exp, exp_opt};
use crate::parser::{ErrorKind, Exited, Expected, Parser};
use crate::pat::{at_pat, pat};
use crate::ty::{of_ty, ty, ty_annotation, ty_var_seq};
use crate::util::{
  ascription, eat_name_star, many_sep, maybe_semi_sep, name_star_eq, path, path_must, InfixErr,
};
use sml_syntax::kind::SyntaxKind as SK;

pub(crate) fn dec(p: &mut Parser<'_>, fe: &mut sml_fixity::Env, infix: InfixErr) -> bool {
  maybe_semi_sep(p, SK::Dec, |p| dec_with_tail(p, fe, infix))
}

fn dec_one(p: &mut Parser<'_>, fe: &mut sml_fixity::Env, infix: InfixErr) -> bool {
  let kind = match p.peek() {
    Some(t) => t.kind,
    None => return false,
  };
  let en = p.enter();
  let kind = match kind {
    SK::DotDotDot => {
      p.bump();
      SK::HoleDec
    }
    SK::ValKw => {
      p.bump();
      ty_var_seq(p);
      many_sep(p, SK::AndKw, SK::ValBind, |p| {
        let mut got = false;
        if p.at(SK::RecKw) {
          p.bump();
          got = true;
        }
        if pat(p, fe, infix).is_some() {
          got = true;
        } else {
          p.error(ErrorKind::Expected(Expected::Pat));
        }
        if !got {
          return false;
        }
        eq_exp(p, fe);
        true
      });
      SK::ValDec
    }
    SK::FunKw => {
      p.bump();
      ty_var_seq(p);
      many_sep(p, SK::AndKw, SK::FunBind, |p| {
        if p.at(SK::Bar) {
          p.bump();
        }
        many_sep(p, SK::Bar, SK::FunBindCase, |p| {
          let en = p.enter();
          let save = p.save();
          infix_fun_bind_case_head_inner(p, fe, infix);
          if p.ok_since(save) {
            p.exit(en, SK::InfixFunBindCaseHead);
            // this is the case where the () around the infix fun bind case head are dropped. thus,
            // there are no other at_pats allowed. `ty_annotation` or `=` must immediately follow.
          } else {
            if p.at(SK::LRound) {
              p.bump();
              infix_fun_bind_case_head_inner(p, fe, infix);
              p.eat(SK::RRound);
              p.exit(en, SK::InfixFunBindCaseHead);
            } else {
              let saw_op = p.at(SK::OpKw);
              let name = p.peek_n(usize::from(saw_op));
              let is_name_star = name.is_some_and(|tok| matches!(tok.kind, SK::Name | SK::Star));
              let infix_name = name
                .and_then(|tok| fe.contains_key(tok.text).then(|| str_util::Name::new(tok.text)));
              if saw_op {
                if is_name_star && infix_name.is_none() {
                  p.error(ErrorKind::UnnecessaryOp);
                }
                p.bump();
              }
              if is_name_star {
                if !saw_op {
                  if let Some(name) = infix_name {
                    p.error(ErrorKind::InfixWithoutOp(name));
                  }
                }
                p.bump();
              } else {
                p.error(ErrorKind::Expected(Expected::Kind(SK::Name)));
              }
              p.exit(en, SK::PrefixFunBindCaseHead);
            }
            while at_pat(p, fe, infix).is_some() {
              // no body
            }
          }
          _ = ty_annotation(p);
          eq_exp(p, fe);
          true
        })
      });
      SK::FunDec
    }
    SK::TypeKw | SK::EqtypeKw => {
      p.bump();
      ty_binds(p);
      SK::TyDec
    }
    SK::DatatypeKw => match datatype(p, true) {
      Datatype::Regular => SK::DatDec,
      Datatype::Copy => SK::DatCopyDec,
    },
    SK::AbstypeKw => {
      p.bump();
      dat_binds(p, true);
      p.eat(SK::WithKw);
      dec(p, fe, infix);
      p.eat(SK::EndKw);
      SK::AbstypeDec
    }
    SK::ExceptionKw => {
      p.bump();
      many_sep(p, SK::AndKw, SK::ExBind, |p| {
        let mut got = false;
        if p.at(SK::OpKw) {
          p.error(ErrorKind::UnnecessaryOp);
          p.bump();
          got = true;
        }
        got |= eat_name_star(p).is_some();
        if !got {
          return false;
        }
        if of_ty(p).is_none() {
          eq_exp(p, fe);
        }
        true
      });
      SK::ExDec
    }
    SK::OpenKw => {
      p.bump();
      while path(p).is_some() {
        // no body
      }
      SK::OpenDec
    }
    SK::InfixKw => {
      p.bump();
      let num = fixity(p);
      names_star_eq(p, |name| {
        fe.insert(str_util::Name::new(name), sml_fixity::Infix::left(num));
      });
      SK::InfixDec
    }
    SK::InfixrKw => {
      p.bump();
      let num = fixity(p);
      names_star_eq(p, |name| {
        fe.insert(str_util::Name::new(name), sml_fixity::Infix::right(num));
      });
      SK::InfixrDec
    }
    SK::NonfixKw => {
      p.bump();
      names_star_eq(p, |name| {
        fe.remove(name);
      });
      SK::NonfixDec
    }
    SK::DoKw => {
      p.bump();
      exp(p, fe);
      SK::DoDec
    }
    SK::LocalKw => {
      let inner = p.enter();
      p.bump();
      dec(p, fe, infix);
      p.exit(inner, SK::LocalDecHd);
      let inner = p.enter();
      p.eat(SK::InKw);
      dec(p, fe, infix);
      p.exit(inner, SK::LocalDecTl);
      p.eat(SK::EndKw);
      SK::LocalDec
    }
    SK::StructureKw => {
      p.bump();
      many_sep(p, SK::AndKw, SK::StrBind, |p| {
        if p.eat(SK::Name).is_none() {
          return false;
        }
        if ascription(p) {
          ascription_tail(p, fe);
        }
        if p.at(SK::Eq) {
          let en = p.enter();
          p.bump();
          if str_exp(p, fe).is_none() {
            p.error(ErrorKind::Expected(Expected::StrExp));
          }
          p.exit(en, SK::EqStrExp);
        }
        true
      });
      SK::StructureDec
    }
    SK::SignatureKw => {
      p.bump();
      many_sep(p, SK::AndKw, SK::SigBind, |p| {
        if p.eat(SK::Name).is_none() {
          return false;
        }
        p.eat(SK::Eq);
        if sig_exp(p, fe).is_none() {
          p.error(ErrorKind::Expected(Expected::SigExp));
        }
        true
      });
      SK::SignatureDec
    }
    SK::FunctorKw => {
      p.bump();
      many_sep(p, SK::AndKw, SK::FunctorBind, |p| {
        if p.eat(SK::Name).is_none() {
          return false;
        }
        p.eat(SK::LRound);
        if p.at(SK::Name) {
          let en = p.enter();
          p.bump();
          if ascription(p) {
            p.bump();
          }
          if sig_exp(p, fe).is_none() {
            p.error(ErrorKind::Expected(Expected::SigExp));
          }
          p.exit(en, SK::FunctorArgNameSigExp);
        } else {
          dec(p, fe, InfixErr::No);
        }
        p.eat(SK::RRound);
        if ascription(p) {
          ascription_tail(p, fe);
        }
        p.eat(SK::Eq);
        if str_exp(p, fe).is_none() {
          p.error(ErrorKind::Expected(Expected::StrExp));
        }
        true
      });
      SK::FunctorDec
    }
    SK::IncludeKw => {
      p.bump();
      while sig_exp(p, fe).is_some() {
        // no body
      }
      SK::IncludeDec
    }
    _ => {
      if exp_opt(p, fe) {
        SK::ExpDec
      } else {
        p.abandon(en);
        return false;
      }
    }
  };
  p.exit(en, kind);
  true
}

fn str_exp(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> Option<Exited> {
  let kind = p.peek()?.kind;
  let en = p.enter();
  let kind = match kind {
    SK::StructKw => {
      p.bump();
      let mut fe = fe.clone();
      dec(p, &mut fe, InfixErr::Yes);
      p.eat(SK::EndKw);
      SK::StructStrExp
    }
    SK::LetKw => {
      p.bump();
      let mut fe = fe.clone();
      dec(p, &mut fe, InfixErr::Yes);
      p.eat(SK::InKw);
      if str_exp(p, &fe).is_none() {
        p.error(ErrorKind::Expected(Expected::StrExp));
      }
      p.eat(SK::EndKw);
      SK::LetStrExp
    }
    SK::Name => {
      if p.at_n(1, SK::LRound) {
        p.bump();
        p.eat(SK::LRound);
        let arg = p.enter();
        if str_exp(p, fe).is_some() {
          p.exit(arg, SK::AppStrExpArgStrExp);
        } else {
          p.abandon(arg);
          let mut fe = fe.clone();
          dec(p, &mut fe, InfixErr::Yes);
        }
        p.eat(SK::RRound);
        SK::AppStrExp
      } else {
        path_must(p);
        SK::PathStrExp
      }
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  let mut ex = p.exit(en, kind);
  while ascription(p) {
    let en = p.precede(ex);
    ascription_tail(p, fe);
    ex = p.exit(en, SK::AscriptionStrExp);
  }
  Some(ex)
}

/// should have just gotten `true` from [`ascription`]
fn ascription_tail(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> Exited {
  let en = p.enter();
  p.bump();
  if sig_exp(p, fe).is_none() {
    p.error(ErrorKind::Expected(Expected::SigExp));
  }
  p.exit(en, SK::AscriptionTail)
}

fn sig_exp(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> Option<Exited> {
  let kind = p.peek()?.kind;
  let en = p.enter();
  let kind = match kind {
    SK::SigKw => {
      p.bump();
      let mut fe = fe.clone();
      dec(p, &mut fe, InfixErr::No);
      p.eat(SK::EndKw);
      SK::SigSigExp
    }
    SK::Name => {
      p.bump();
      SK::NameSigExp
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  let mut ex = p.exit(en, kind);
  // the first 'where' must actually be 'where', but further ones can be 'and'.
  if !p.at(SK::WhereKw) {
    return Some(ex);
  }
  while p.at(SK::WhereKw) || p.at(SK::AndKw) {
    let en = p.precede(ex);
    p.bump();
    if p.at(SK::TypeKw) {
      p.bump();
      ty_var_seq(p);
      path_must(p);
      p.eat(SK::Eq);
      ty(p);
      ex = p.exit(en, SK::WhereTypeSigExp);
    } else {
      path_must(p);
      p.eat(SK::Eq);
      path_must(p);
      ex = p.exit(en, SK::WhereSigExp);
    }
  }
  Some(ex)
}

fn dec_with_tail(p: &mut Parser<'_>, fe: &mut sml_fixity::Env, infix: InfixErr) -> bool {
  let en = p.enter();
  let mut ret = maybe_semi_sep(p, SK::DecInSeq, |p| dec_one(p, fe, infix));
  while p.at(SK::SharingKw) {
    ret = true;
    let en = p.enter();
    p.bump();
    if p.at(SK::TypeKw) {
      p.bump();
    }
    many_sep(p, SK::Eq, SK::PathEq, path_must);
    p.exit(en, SK::SharingTail);
  }
  p.exit(en, SK::DecWithTail);
  ret
}

fn fixity(p: &mut Parser<'_>) -> u16 {
  let mut ret = 0;
  if p.at(SK::IntLit) {
    let text = p.peek().unwrap().text;
    if text.starts_with('~') {
      p.error(ErrorKind::NegativeFixity);
    } else {
      match text.parse::<u16>() {
        Ok(x) => ret = x,
        Err(e) => p.error(ErrorKind::InvalidFixity(e)),
      }
    }
    p.bump();
  } else if !name_star_eq(p) {
    p.error(ErrorKind::Expected(Expected::NameOrInt));
  }
  ret
}

fn names_star_eq<F>(p: &mut Parser<'_>, mut f: F)
where
  F: FnMut(&str),
{
  while name_star_eq(p) {
    let text = p.bump().text;
    f(text);
  }
}

enum Datatype {
  Regular,
  Copy,
}

/// we just saw a `datatype` keyword.
fn datatype(p: &mut Parser<'_>, allow_op: bool) -> Datatype {
  p.bump();
  if p.at_n(2, SK::DatatypeKw) {
    p.eat(SK::Name);
    p.eat(SK::Eq);
    p.eat(SK::DatatypeKw);
    path_must(p);
    Datatype::Copy
  } else {
    dat_binds(p, allow_op);
    Datatype::Regular
  }
}

fn dat_binds(p: &mut Parser<'_>, allow_op: bool) {
  many_sep(p, SK::AndKw, SK::DatBind, |p| {
    // use `&` not `&&` to prevent short circuit
    if !ty_var_seq(p) & p.eat(SK::Name).is_none() {
      return false;
    }
    if p.at(SK::Eq) {
      let en = p.enter();
      p.bump();
      if p.at(SK::Bar) {
        p.bump();
      }
      many_sep(p, SK::Bar, SK::ConBind, |p| {
        let mut got = false;
        if allow_op && p.at(SK::OpKw) {
          p.error(ErrorKind::UnnecessaryOp);
          p.bump();
          got = true;
        }
        got |= eat_name_star(p).is_some();
        if !got {
          return false;
        }
        _ = of_ty(p);
        true
      });
      p.exit(en, SK::EqConBinds);
    }
    true
  });
  if p.at(SK::WithtypeKw) {
    let en = p.enter();
    p.bump();
    ty_binds(p);
    p.exit(en, SK::WithType);
  }
}

fn ty_binds(p: &mut Parser<'_>) {
  many_sep(p, SK::AndKw, SK::TyBind, |p| {
    // use `&` not `&&` to prevent short circuit
    if !ty_var_seq(p) & p.eat(SK::Name).is_none() {
      return false;
    }
    if p.at(SK::Eq) {
      let en = p.enter();
      p.bump();
      ty(p);
      p.exit(en, SK::EqTy);
    }
    true
  });
}

fn infix_fun_bind_case_head_inner(p: &mut Parser<'_>, fe: &sml_fixity::Env, infix: InfixErr) {
  if at_pat(p, fe, infix).is_none() {
    p.error(ErrorKind::Expected(Expected::Pat));
  }
  if let Some(name) = eat_name_star(p) {
    if !fe.contains_key(name.text) {
      p.error(ErrorKind::NotInfix(str_util::Name::new(name.text)));
    }
  }
  if at_pat(p, fe, infix).is_none() {
    p.error(ErrorKind::Expected(Expected::Pat));
  }
}
