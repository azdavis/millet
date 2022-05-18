use crate::dec::dec;
use crate::parser::{Exited, Expected, OpInfo, Parser};
use crate::pat::pat;
use crate::ty::ty;
use crate::util::{comma_sep, lab, many_sep, must, path, scon, should_break};
use syntax::SyntaxKind as SK;

pub(crate) fn exp(p: &mut Parser<'_>) {
  must(p, |p| exp_prec(p, ExpPrec::Min), Expected::Exp)
}

#[must_use]
fn exp_prec(p: &mut Parser<'_>, min_prec: ExpPrec) -> Option<Exited> {
  let ent = p.enter();
  let ex = if p.at(SK::RaiseKw) {
    p.bump();
    exp(p);
    p.exit(ent, SK::RaiseExp)
  } else if p.at(SK::IfKw) {
    p.bump();
    exp(p);
    p.eat(SK::ThenKw);
    exp(p);
    p.eat(SK::ElseKw);
    exp(p);
    p.exit(ent, SK::IfExp)
  } else if p.at(SK::WhileKw) {
    p.bump();
    exp(p);
    p.eat(SK::DoKw);
    exp(p);
    p.exit(ent, SK::WhileExp)
  } else if p.at(SK::CaseKw) {
    p.bump();
    exp(p);
    p.eat(SK::OfKw);
    matcher(p);
    p.exit(ent, SK::CaseExp)
  } else if p.at(SK::FnKw) {
    p.bump();
    matcher(p);
    p.exit(ent, SK::FnExp)
  } else {
    p.abandon(ent);
    let mut ex = at_exp(p)?;
    loop {
      let op_info = p.peek().and_then(|tok| {
        // NOTE: `*` and `=` are special because they are 'proper' tokens (`*` in tuple types, `=`
        // in e.g. val bindings), but they can also be regular operator names (and indeed are, in
        // the standard basis).
        if matches!(tok.kind, SK::Name | SK::Star | SK::Eq) {
          p.get_op(tok.text)
        } else {
          None
        }
      });
      ex = if let Some(op_info) = op_info {
        if should_break_exp(p, ExpPrec::Infix(op_info), min_prec) {
          break;
        }
        let ent = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Infix(op_info)), Expected::Exp);
        p.exit(ent, SK::InfixExp)
      } else if p.at(SK::Colon) {
        if matches!(min_prec, ExpPrec::Infix(_)) {
          break;
        }
        let ent = p.precede(ex);
        p.bump();
        ty(p);
        p.exit(ent, SK::TypedExp)
      } else if p.at(SK::AndalsoKw) {
        if should_break_exp(p, ExpPrec::Andalso, min_prec) {
          break;
        }
        let ent = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Andalso), Expected::Exp);
        p.exit(ent, SK::AndalsoExp)
      } else if p.at(SK::OrelseKw) {
        if should_break_exp(p, ExpPrec::Orelse, min_prec) {
          break;
        }
        let ent = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Orelse), Expected::Exp);
        p.exit(ent, SK::OrelseExp)
      } else if p.at(SK::HandleKw) {
        let ent = p.precede(ex);
        p.bump();
        matcher(p);
        p.exit(ent, SK::HandleExp)
      } else if at_exp_hd(p) {
        let ent = p.precede(ex);
        must(p, at_exp, Expected::Exp);
        p.exit(ent, SK::AppExp)
      } else {
        break;
      };
    }
    ex
  };
  Some(ex)
}

/// when adding more cases to this, update [`at_exp_hd`]
#[must_use]
fn at_exp(p: &mut Parser<'_>) -> Option<Exited> {
  let ent = p.enter();
  let ex = if scon(p) {
    p.bump();
    p.exit(ent, SK::SConExp)
  } else if p.at(SK::OpKw) {
    p.bump();
    must(p, path, Expected::Path);
    p.exit(ent, SK::PathExp)
  } else if p.at(SK::Name) {
    must(p, path, Expected::Path);
    p.exit(ent, SK::PathExp)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::ExpRow, |p| {
      must(p, lab, Expected::Lab);
      p.eat(SK::Eq);
      exp(p);
    });
    p.exit(ent, SK::RecordExp)
  } else if p.at(SK::Hash) {
    p.bump();
    must(p, lab, Expected::Lab);
    p.exit(ent, SK::SelectorExp)
  } else if p.at(SK::LRound) {
    p.bump();
    let one = comma_sep(p, SK::RRound, SK::ExpArg, |p| {
      exp(p);
    });
    p.exit(ent, if one { SK::ParenExp } else { SK::TupleExp })
  } else if p.at(SK::LSquare) {
    p.bump();
    comma_sep(p, SK::RSquare, SK::ExpArg, |p| {
      exp(p);
    });
    p.exit(ent, SK::ListExp)
  } else if p.at(SK::LetKw) {
    p.bump();
    dec(p);
    p.eat(SK::InKw);
    many_sep(p, SK::Semicolon, SK::ExpInSeq, |p| {
      exp(p);
    });
    p.eat(SK::EndKw);
    p.exit(ent, SK::LetExp)
  } else {
    p.abandon(ent);
    return None;
  };
  Some(ex)
}

fn matcher(p: &mut Parser<'_>) {
  many_sep(p, SK::Bar, SK::MatchRule, |p| {
    must(p, pat, Expected::Pat);
    p.eat(SK::EqGt);
    exp(p);
  });
}

/// need this for app expressions to know whether to precede or not.
fn at_exp_hd(p: &mut Parser<'_>) -> bool {
  scon(p)
    || p.at(SK::OpKw)
    || p.at(SK::Name)
    || p.at(SK::LCurly)
    || p.at(SK::Hash)
    || p.at(SK::LRound)
    || p.at(SK::LSquare)
    || p.at(SK::LetKw)
}

#[derive(Debug, Clone, Copy)]
enum ExpPrec {
  Infix(OpInfo),
  Andalso,
  Orelse,
  Min,
}

fn should_break_exp(p: &mut Parser<'_>, prec: ExpPrec, min_prec: ExpPrec) -> bool {
  match (prec, min_prec) {
    (ExpPrec::Infix(prec), ExpPrec::Infix(min_prec)) => should_break(p, prec, Some(min_prec)),
    (ExpPrec::Andalso, ExpPrec::Andalso) | (ExpPrec::Orelse, ExpPrec::Orelse) => true,
    (ExpPrec::Infix(_) | ExpPrec::Andalso | ExpPrec::Orelse, _) => false,
    (ExpPrec::Min, _) => unreachable!(),
  }
}
