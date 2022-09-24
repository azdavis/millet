use crate::exp::exp;
use crate::parser::{ErrorKind, Exited, Expected, Infix, Parser};
use crate::pat::{at_pat, pat};
use crate::ty::{of_ty, ty, ty_annotation, ty_var_seq};
use crate::util::{eat_name_star, many_sep, maybe_semi_sep, must, name_star_eq, path};
use sml_syntax::SyntaxKind as SK;

pub(crate) fn str_dec(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  let ret = maybe_semi_sep(p, SK::StrDecInSeq, str_dec_one);
  p.exit(en, SK::StrDec);
  ret
}

fn str_dec_one(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  if p.at(SK::LocalKw) {
    // LocalStrDec is a 'superset' of LocalDec, so always use the former
    p.bump();
    str_dec(p);
    p.eat(SK::InKw);
    str_dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::LocalStrDec);
  } else if p.at(SK::StructureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::StrBind, |p| {
      if p.eat(SK::Name).is_none() {
        return false;
      }
      if ascription(p) {
        ascription_tail(p);
      }
      p.eat(SK::Eq);
      must(p, str_exp, Expected::StrExp);
      true
    });
    p.exit(en, SK::StructureStrDec);
  } else if p.at(SK::SignatureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::SigBind, |p| {
      if p.eat(SK::Name).is_none() {
        return false;
      }
      p.eat(SK::Eq);
      must(p, sig_exp, Expected::SigExp);
      true
    });
    p.exit(en, SK::SigDec);
  } else if p.at(SK::FunctorKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::FunctorBind, |p| {
      if p.eat(SK::Name).is_none() {
        return false;
      }
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
      true
    });
    p.exit(en, SK::FunctorDec);
  } else if dec_one(p) {
    p.exit(en, SK::DecStrDec);
  } else {
    p.abandon(en);
    return false;
  }
  true
}

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
    if p.at(SK::TypeKw) {
      p.bump();
      ty_var_seq(p);
      must(p, path, Expected::Path);
      p.eat(SK::Eq);
      ty(p);
      ex = p.exit(en, SK::WhereTypeSigExp);
    } else {
      must(p, path, Expected::Path);
      p.eat(SK::Eq);
      must(p, path, Expected::Path);
      ex = p.exit(en, SK::WhereSigExp);
    }
  }
  Some(ex)
}

fn spec_one(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  if p.at(SK::ValKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ValDesc, |p| {
      if eat_name_star(p).is_none() {
        return false;
      }
      p.eat(SK::Colon);
      ty(p);
      true
    });
    p.exit(en, SK::ValSpec);
  } else if p.at(SK::TypeKw) {
    ty_spec(p);
    p.exit(en, SK::TySpec);
  } else if p.at(SK::EqtypeKw) {
    ty_spec(p);
    p.exit(en, SK::EqTySpec);
  } else if p.at(SK::DatatypeKw) {
    match datatype(p, false) {
      Datatype::Regular => p.exit(en, SK::DatSpec),
      Datatype::Copy => p.exit(en, SK::DatCopySpec),
    };
  } else if p.at(SK::ExceptionKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ExDesc, |p| {
      if eat_name_star(p).is_none() {
        return false;
      }
      let _ = of_ty(p);
      true
    });
    p.exit(en, SK::ExSpec);
  } else if p.at(SK::StructureKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::StrDesc, |p| {
      if p.eat(SK::Name).is_none() {
        return false;
      }
      p.eat(SK::Colon);
      must(p, sig_exp, Expected::SigExp);
      true
    });
    p.exit(en, SK::StrSpec);
  } else if p.at(SK::IncludeKw) {
    p.bump();
    while sig_exp(p).is_some() {
      // no body
    }
    p.exit(en, SK::IncludeSpec);
  } else {
    p.abandon(en);
    return false;
  }
  true
}

fn ty_spec(p: &mut Parser<'_>) {
  p.bump();
  many_sep(p, SK::AndKw, SK::TyDesc, |p| {
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

fn spec_with_tail(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  let mut ret = maybe_semi_sep(p, SK::SpecInSeq, spec_one);
  while p.at(SK::SharingKw) {
    ret = true;
    let en = p.enter();
    p.bump();
    if p.at(SK::TypeKw) {
      p.bump();
    }
    many_sep(p, SK::Eq, SK::PathEq, |p| must(p, path, Expected::Path));
    p.exit(en, SK::SharingTail);
  }
  p.exit(en, SK::SpecWithTail);
  ret
}

fn spec(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  maybe_semi_sep(p, SK::SpecWithTailInSeq, spec_with_tail);
  p.exit(en, SK::Spec)
}

pub(crate) fn dec(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  maybe_semi_sep(p, SK::DecInSeq, dec_one);
  p.exit(en, SK::Dec)
}

fn dec_one(p: &mut Parser<'_>) -> bool {
  let en = p.enter();
  if p.at(SK::DotDotDot) {
    p.bump();
    p.exit(en, SK::HoleDec);
  } else if p.at(SK::ValKw) {
    p.bump();
    ty_var_seq(p);
    many_sep(p, SK::AndKw, SK::ValBind, |p| {
      let mut got = false;
      if p.at(SK::RecKw) {
        p.bump();
        got = true;
      }
      got |= must(p, pat, Expected::Pat);
      if !got {
        return false;
      }
      p.eat(SK::Eq);
      exp(p);
      true
    });
    p.exit(en, SK::ValDec);
  } else if p.at(SK::FunKw) {
    p.bump();
    ty_var_seq(p);
    if p.at(SK::Bar) {
      p.bump();
    }
    many_sep(p, SK::AndKw, SK::FunBind, |p| {
      many_sep(p, SK::Bar, SK::FunBindCase, |p| {
        let en = p.enter();
        let save = p.save();
        infix_fun_bind_case_head_inner(p);
        if p.ok_since(save) {
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
              if !saw_op && p.is_infix(name.text) {
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
        true
      })
    });
    p.exit(en, SK::FunDec);
  } else if p.at(SK::TypeKw) {
    p.bump();
    ty_binds(p);
    p.exit(en, SK::TyDec);
  } else if p.at(SK::DatatypeKw) {
    match datatype(p, true) {
      Datatype::Regular => p.exit(en, SK::DatDec),
      Datatype::Copy => p.exit(en, SK::DatCopyDec),
    };
  } else if p.at(SK::AbstypeKw) {
    p.bump();
    dat_binds(p, true);
    p.eat(SK::WithKw);
    dec(p);
    p.eat(SK::EndKw);
    p.exit(en, SK::AbstypeDec);
  } else if p.at(SK::ExceptionKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ExBind, |p| {
      let mut got = false;
      if p.at(SK::OpKw) {
        p.bump();
        got = true;
      }
      got |= eat_name_star(p).is_some();
      if !got {
        return false;
      }
      if of_ty(p).is_none() && p.at(SK::Eq) {
        let en = p.enter();
        p.bump();
        must(p, path, Expected::Path);
        p.exit(en, SK::EqPath);
      }
      true
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
    names_star_eq(p, |p, name| p.insert_infix(name, Infix::left(num)));
    p.exit(en, SK::InfixDec);
  } else if p.at(SK::InfixrKw) {
    p.bump();
    let num = fixity(p);
    names_star_eq(p, |p, name| p.insert_infix(name, Infix::right(num)));
    p.exit(en, SK::InfixrDec);
  } else if p.at(SK::NonfixKw) {
    p.bump();
    names_star_eq(p, |p, name| p.remove_infix(name));
    p.exit(en, SK::NonfixDec);
  } else if p.at(SK::DoKw) {
    p.bump();
    exp(p);
    p.exit(en, SK::DoDec);
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
    p.error(ErrorKind::Expected(Expected::Kind(SK::Name)));
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
    must(p, path, Expected::Path);
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
    p.eat(SK::Eq);
    if p.at(SK::Bar) {
      p.bump();
    }
    many_sep(p, SK::Bar, SK::ConBind, |p| {
      let mut got = false;
      if allow_op && p.at(SK::OpKw) {
        p.bump();
        got = true
      }
      got |= eat_name_star(p).is_some();
      if !got {
        return false;
      }
      let _ = of_ty(p);
      true
    });
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
    p.eat(SK::Eq);
    ty(p);
    true
  });
}

fn infix_fun_bind_case_head_inner(p: &mut Parser<'_>) {
  must(p, at_pat, Expected::Pat);
  if let Some(name) = eat_name_star(p) {
    if !p.is_infix(name.text) {
      p.error(ErrorKind::NotInfix);
    }
  }
  must(p, at_pat, Expected::Pat);
}
