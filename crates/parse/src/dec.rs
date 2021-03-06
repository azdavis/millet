use crate::exp::exp;
use crate::parser::{ErrorKind, Exited, OpInfo, Parser};
use crate::pat::{at_pat, pat};
use crate::ty::{of_ty, ty, ty_annotation, ty_var_seq};
use crate::util::{many_sep, maybe_semi_sep, must, path};
use syntax::SyntaxKind as SK;

pub(crate) fn dec(p: &mut Parser<'_>) -> Exited {
  let ent = p.enter();
  maybe_semi_sep(p, SK::DecInSeq, dec_one);
  p.exit(ent, SK::Dec)
}

#[must_use]
fn dec_one(p: &mut Parser<'_>) -> Option<Exited> {
  let ent = p.enter();
  let ex = if p.at(SK::ValKw) {
    p.bump();
    ty_var_seq(p);
    many_sep(p, SK::AndKw, SK::ValBind, |p| {
      if p.at(SK::RecKw) {
        p.bump();
      }
      must(p, pat);
      p.eat(SK::Eq);
      exp(p);
    });
    p.exit(ent, SK::ValDec)
  } else if p.at(SK::FunKw) {
    p.bump();
    ty_var_seq(p);
    many_sep(p, SK::AndKw, SK::FunBind, |p| {
      many_sep(p, SK::Bar, SK::FunBindCase, |p| {
        let ent = p.enter();
        let save = p.save();
        prefix_fun_bind_case_head_inner(p);
        if p.error_since(&save) {
          p.restore(save);
          if p.at(SK::LRound) {
            p.bump();
            prefix_fun_bind_case_head_inner(p);
            p.eat(SK::RRound);
            p.exit(ent, SK::InfixFunBindCaseHead);
          } else {
            let saw_op = p.at(SK::OpKw);
            if saw_op {
              p.bump();
            }
            if let Some(name) = p.eat(SK::Name) {
              if !saw_op && p.contains_op(name.text) {
                p.error_with(ErrorKind::InfixWithoutOp);
              }
            }
            p.exit(ent, SK::InfixFunBindCaseHead);
          }
          while at_pat(p).is_some() {
            // no body
          }
        } else {
          p.exit(ent, SK::InfixFunBindCaseHead);
          // in the case where the () are dropped, there are no other at_pats allowed.
          // `ty_annotation` or `=` must immediately follow.
        }
        let _ = ty_annotation(p);
        p.eat(SK::Eq);
        exp(p);
      })
    });
    p.exit(ent, SK::FunDec)
  } else if p.at(SK::TypeKw) {
    p.bump();
    ty_binds(p);
    p.exit(ent, SK::TyDec)
  } else if p.at(SK::DatatypeKw) {
    // TODO datatype copy dec
    p.bump();
    dat_binds_with_type(p);
    p.exit(ent, SK::DatDec)
  } else if p.at(SK::AbstypeKw) {
    p.bump();
    dat_binds_with_type(p);
    p.eat(SK::WithKw);
    dec(p);
    p.eat(SK::EndKw);
    p.exit(ent, SK::AbstypeDec)
  } else if p.at(SK::ExceptionKw) {
    p.bump();
    many_sep(p, SK::AndKw, SK::ExBind, |p| {
      if p.at(SK::OpKw) {
        p.bump();
      }
      p.eat(SK::Name);
      if of_ty(p).is_none() && p.at(SK::Eq) {
        let ent = p.enter();
        p.bump();
        must(p, path);
        p.exit(ent, SK::EqPath);
      }
    });
    p.exit(ent, SK::ExDec)
  } else if p.at(SK::LocalKw) {
    p.bump();
    dec(p);
    p.eat(SK::InKw);
    dec(p);
    p.eat(SK::EndKw);
    p.exit(ent, SK::LocalDec)
  } else if p.at(SK::OpenKw) {
    p.bump();
    while path(p).is_some() {
      // no body
    }
    p.exit(ent, SK::OpenDec)
  } else if p.at(SK::InfixKw) {
    p.bump();
    let num = fixity(p);
    for name in names(p) {
      p.insert_op(name, OpInfo::left(num));
    }
    p.exit(ent, SK::InfixDec)
  } else if p.at(SK::InfixrKw) {
    p.bump();
    let num = fixity(p);
    for name in names(p) {
      p.insert_op(name, OpInfo::right(num));
    }
    p.exit(ent, SK::InfixrDec)
  } else if p.at(SK::NonfixKw) {
    p.bump();
    for name in names(p) {
      p.remove_op(name);
    }
    p.exit(ent, SK::NonfixDec)
  } else {
    p.abandon(ent);
    return None;
  };
  Some(ex)
}

fn fixity(p: &mut Parser<'_>) -> usize {
  if p.at(SK::IntLit) {
    match p.bump().text.parse::<usize>() {
      Ok(x) => x,
      Err(e) => {
        p.error_with(ErrorKind::InvalidFixity(e));
        0
      }
    }
  } else {
    0
  }
}

fn names<'a>(p: &mut Parser<'a>) -> Vec<&'a str> {
  let mut ret = Vec::new();
  while p.at(SK::Name) {
    ret.push(p.bump().text);
  }
  if ret.is_empty() {
    p.error();
  }
  ret
}

fn dat_binds_with_type(p: &mut Parser<'_>) {
  many_sep(p, SK::AndKw, SK::DatBind, |p| {
    ty_var_seq(p);
    p.eat(SK::Name);
    p.eat(SK::Eq);
    many_sep(p, SK::Bar, SK::ConBind, |p| {
      if p.at(SK::OpKw) {
        p.bump();
      }
      p.eat(SK::Name);
      let _ = of_ty(p);
    });
  });
  if p.at(SK::WithtypeKw) {
    let ent = p.enter();
    p.bump();
    ty_binds(p);
    p.exit(ent, SK::WithType);
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

fn prefix_fun_bind_case_head_inner(p: &mut Parser<'_>) {
  must(p, at_pat);
  if let Some(name) = p.eat(SK::Name) {
    if !p.contains_op(name.text) {
      p.error_with(ErrorKind::NotInfix);
    }
  }
  must(p, at_pat);
}
