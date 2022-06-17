use crate::exp::exp;
use crate::parser::{ErrorKind, Exited, Expected, OpInfo, Parser};
use crate::pat::{at_pat, pat};
use crate::ty::{of_ty, ty, ty_annotation, ty_var_seq};
use crate::util::{eat_name_star, many_sep, maybe_semi_sep, must, name_star_eq, path};
use syntax::SyntaxKind as SK;

pub(crate) fn dec(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  maybe_semi_sep(p, SK::DecInSeq, dec_one);
  p.exit(en, SK::Dec)
}

#[must_use]
pub(crate) fn dec_one(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  if p.at(SK::ValKw) {
    p.bump();
    ty_var_seq(p);
    many_sep(p, SK::AndKw, SK::ValBind, |p| {
      if p.at(SK::RecKw) {
        p.bump();
      }
      must(p, pat, Expected::Pat);
      p.eat(SK::Eq);
      exp(p);
    });
    p.exit(en, SK::ValDec);
  } else if p.at(SK::FunKw) {
    p.bump();
    ty_var_seq(p);
    many_sep(p, SK::AndKw, SK::FunBind, |p| {
      many_sep(p, SK::Bar, SK::FunBindCase, |p| {
        let en = p.enter();
        let save = p.save();
        infix_fun_bind_case_head_inner(p);
        if p.maybe_discard(save) {
          p.exit(en, SK::InfixFunBindCaseHead);
          // this is the case where the () around the infix fun bind case head are dropped. thus,
          // there are no other at_pats allowed. `ty_annotation` or `=` must immediately follow.
        } else {
          if p.at(SK::LRound) {
            p.bump();
            infix_fun_bind_case_head_inner(p);
            p.eat(SK::RRound);
            p.exit(en, SK::InfixFunBindCaseHead);
          } else {
            let saw_op = p.at(SK::OpKw);
            if saw_op {
              p.bump();
            }
            if let Some(name) = eat_name_star(p) {
              if !saw_op && p.contains_op(name.text) {
                p.error(ErrorKind::InfixWithoutOp);
              }
            }
            p.exit(en, SK::PrefixFunBindCaseHead);
          }
          while at_pat(p).is_some() {
            // no body
          }
        }
        let _ = ty_annotation(p);
        p.eat(SK::Eq);
        exp(p);
      })
    });
    p.exit(en, SK::FunDec);
  } else if p.at(SK::TypeKw) {
    p.bump();
    ty_binds(p);
    p.exit(en, SK::TyDec);
  } else if p.at(SK::DatatypeKw) {
    if datatype_copy(p) {
      p.exit(en, SK::DatCopyDec);
    } else {
      dat_binds_with_type(p);
      p.exit(en, SK::DatDec);
    }
  } else if p.at(SK::AbstypeKw) {
    p.bump();
    dat_binds_with_type(p);
    p.eat(SK::WithKw);
    dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::AbstypeDec);
  } else if p.at(SK::ExceptionKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ExBind, |p| {
      if p.at(SK::OpKw) {
        p.bump();
      }
      eat_name_star(p);
      if of_ty(p).is_none() && p.at(SK::Eq) {
        let en = p.enter();
        p.bump();
        must(p, path, Expected::Path);
        p.exit(en, SK::EqPath);
      }
    });
    p.exit(en, SK::ExDec);
  } else if p.at(SK::LocalKw) {
    p.bump();
    dec(p);
    p.eat(SK::InKw);
    dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::LocalDec);
  } else if p.at(SK::OpenKw) {
    p.bump();
    while path(p).is_some() {
      // no body
    }
    p.exit(en, SK::OpenDec);
  } else if p.at(SK::InfixKw) {
    p.bump();
    let num = fixity(p);
    names_star_eq(p, |p, name| p.insert_op(name, OpInfo::left(num)));
    p.exit(en, SK::InfixDec);
  } else if p.at(SK::InfixrKw) {
    p.bump();
    let num = fixity(p);
    names_star_eq(p, |p, name| p.insert_op(name, OpInfo::right(num)));
    p.exit(en, SK::InfixrDec);
  } else if p.at(SK::NonfixKw) {
    p.bump();
    names_star_eq(p, |p, name| p.remove_op(name));
    p.exit(en, SK::NonfixDec);
  } else {
    p.abandon(en);
    return false;
  };
  true
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
  }
  ret
}

fn names_star_eq<'a, F>(p: &mut Parser<'a>, mut f: F)
where
  F: FnMut(&mut Parser<'a>, &'a str),
{
  let mut got = false;
  while name_star_eq(p) {
    got = true;
    let text = p.bump().text;
    f(p, text);
  }
  if !got {
    p.error(ErrorKind::ExpectedKind(SK::Name));
  }
}

pub(crate) fn dat_binds(p: &mut Parser<'_>, allow_op: bool) {
  many_sep(p, SK::AndKw, SK::DatBind, |p| {
    ty_var_seq(p);
    p.eat(SK::Name);
    p.eat(SK::Eq);
    many_sep(p, SK::Bar, SK::ConBind, |p| {
      if allow_op && p.at(SK::OpKw) {
        p.bump();
      }
      eat_name_star(p);
      let _ = of_ty(p);
    });
  });
}

fn dat_binds_with_type(p: &mut Parser<'_>) {
  dat_binds(p, true);
  if p.at(SK::WithtypeKw) {
    let en = p.enter();
    p.bump();
    ty_binds(p);
    p.exit(en, SK::WithType);
  }
}

fn ty_binds(p: &mut Parser<'_>) {
  many_sep(p, SK::AndKw, SK::TyBind, |p| {
    ty_var_seq(p);
    p.eat(SK::Name);
    p.eat(SK::Eq);
    ty(p);
  });
}

fn infix_fun_bind_case_head_inner(p: &mut Parser<'_>) {
  must(p, at_pat, Expected::Pat);
  if let Some(name) = eat_name_star(p) {
    if !p.contains_op(name.text) {
      p.error(ErrorKind::NotInfix);
    }
  }
  must(p, at_pat, Expected::Pat);
}

/// we just saw a `datatype` keyword starting a dec. this bumps that kw, then tries to parse a
/// datatype copy dec. returns if it was a datatype copy. if false, the state of the parser is reset
/// to right after the bump.
#[must_use]
pub(crate) fn datatype_copy(p: &mut Parser<'_>) -> bool {
  p.bump();
  let save = p.save();
  p.eat(SK::Name);
  p.eat(SK::Eq);
  p.eat(SK::DatatypeKw);
  must(p, path, Expected::Path);
  p.maybe_discard(save)
}
