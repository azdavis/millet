use crate::dec::dec;
use crate::parser::{ErrorKind, Exited, Expected, OpInfo, Parser};
use crate::pat::pat;
use crate::ty::ty;
use crate::util::{
  comma_sep, lab, many_sep, must, name_plus, path, path_no_infix, scon, should_break, ShouldBreak,
};
use syntax::SyntaxKind as SK;

pub(crate) fn exp(p: &mut Parser<'_>) {
  must(p, |p| exp_prec(p, ExpPrec::Min), Expected::Exp)
}

#[must_use]
fn exp_prec(p: &mut Parser<'_>, min_prec: ExpPrec) -> Option<Exited> {
  let en = p.enter();
  let ex = if p.at(SK::RaiseKw) {
    p.bump();
    exp(p);
    p.exit(en, SK::RaiseExp)
  } else if p.at(SK::IfKw) {
    p.bump();
    exp(p);
    p.eat(SK::ThenKw);
    exp(p);
    p.eat(SK::ElseKw);
    exp(p);
    p.exit(en, SK::IfExp)
  } else if p.at(SK::WhileKw) {
    p.bump();
    exp(p);
    p.eat(SK::DoKw);
    exp(p);
    p.exit(en, SK::WhileExp)
  } else if p.at(SK::CaseKw) {
    p.bump();
    exp(p);
    p.eat(SK::OfKw);
    matcher(p);
    p.exit(en, SK::CaseExp)
  } else if p.at(SK::FnKw) {
    p.bump();
    matcher(p);
    p.exit(en, SK::FnExp)
  } else {
    p.abandon(en);
    let mut ex = at_exp(p)?;
    loop {
      let op_info = if name_plus(p) {
        let text = p.peek().unwrap().text;
        p.get_op(text)
      } else {
        None
      };
      ex = if let Some(op_info) = op_info {
        match should_break_exp(ExpPrec::Infix(op_info), min_prec) {
          ShouldBreak::Yes => break,
          ShouldBreak::No => {}
          ShouldBreak::Error => p.error(ErrorKind::SameFixityDiffAssoc),
        }
        let en = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Infix(op_info)), Expected::Exp);
        p.exit(en, SK::InfixExp)
      } else if p.at(SK::Colon) {
        if matches!(min_prec, ExpPrec::Infix(_)) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        ty(p);
        p.exit(en, SK::TypedExp)
      } else if p.at(SK::AndalsoKw) {
        match should_break_exp(ExpPrec::Andalso, min_prec) {
          ShouldBreak::Yes => break,
          ShouldBreak::No => {}
          ShouldBreak::Error => p.error(ErrorKind::SameFixityDiffAssoc),
        }
        let en = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Andalso), Expected::Exp);
        p.exit(en, SK::AndalsoExp)
      } else if p.at(SK::OrelseKw) {
        match should_break_exp(ExpPrec::Orelse, min_prec) {
          ShouldBreak::Yes => break,
          ShouldBreak::No => {}
          ShouldBreak::Error => p.error(ErrorKind::SameFixityDiffAssoc),
        }
        let en = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Orelse), Expected::Exp);
        p.exit(en, SK::OrelseExp)
      } else if p.at(SK::HandleKw) {
        let en = p.precede(ex);
        p.bump();
        matcher(p);
        p.exit(en, SK::HandleExp)
      } else if at_exp_hd(p) {
        let en = p.precede(ex);
        must(p, at_exp, Expected::Exp);
        p.exit(en, SK::AppExp)
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
  let en = p.enter();
  let ex = if scon(p) {
    p.bump();
    p.exit(en, SK::SConExp)
  } else if p.at(SK::OpKw) {
    p.bump();
    must(p, path, Expected::Path);
    p.exit(en, SK::PathExp)
  } else if name_plus(p) {
    path_no_infix(p);
    p.exit(en, SK::PathExp)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::ExpRow, |p| {
      lab(p);
      p.eat(SK::Eq);
      exp(p);
    });
    p.exit(en, SK::RecordExp)
  } else if p.at(SK::Hash) {
    p.bump();
    lab(p);
    p.exit(en, SK::SelectorExp)
  } else if p.at(SK::LRound) {
    p.bump();
    let kind = at_exp_l_round(p);
    p.exit(en, kind)
  } else if p.at(SK::LSquare) {
    p.bump();
    comma_sep(p, SK::RSquare, SK::ExpArg, |p| {
      exp(p);
    });
    p.exit(en, SK::ListExp)
  } else if p.at(SK::LetKw) {
    p.bump();
    dec(p);
    p.eat(SK::InKw);
    many_sep(p, SK::Semicolon, SK::ExpInSeq, |p| {
      exp(p);
    });
    p.eat(SK::EndKw);
    p.exit(en, SK::LetExp)
  } else {
    p.abandon(en);
    return None;
  };
  Some(ex)
}

fn at_exp_l_round(p: &mut Parser<'_>) -> SK {
  if p.at(SK::RRound) {
    p.bump();
    return SK::TupleExp;
  }
  let en = p.enter();
  exp(p);
  if !p.at(SK::Semicolon) && !p.at(SK::Comma) {
    if p.at(SK::RRound) {
      p.abandon(en);
      p.bump();
    } else {
      p.error(ErrorKind::Expected(Expected::LRoundExpTail));
    }
    return SK::ParenExp;
  }
  let kind = p.bump().kind;
  let (wrap, overall) = match kind {
    SK::Semicolon => (SK::ExpInSeq, SK::SeqExp),
    SK::Comma => (SK::ExpArg, SK::TupleExp),
    _ => unreachable!(),
  };
  p.exit(en, wrap);
  loop {
    let en = p.enter();
    exp(p);
    if p.at(kind) {
      p.bump();
      p.exit(en, wrap);
    } else {
      p.exit(en, wrap);
      p.eat(SK::RRound);
      break;
    }
  }
  overall
}

fn matcher(p: &mut Parser<'_>) {
  let en = p.enter();
  many_sep(p, SK::Bar, SK::MatchRule, |p| {
    must(p, pat, Expected::Pat);
    p.eat(SK::EqGt);
    exp(p);
  });
  p.exit(en, SK::Matcher);
}

/// need this for app expressions to know whether to precede or not.
fn at_exp_hd(p: &mut Parser<'_>) -> bool {
  scon(p)
    || p.at(SK::OpKw)
    || name_plus(p)
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

fn should_break_exp(prec: ExpPrec, min_prec: ExpPrec) -> ShouldBreak {
  match (prec, min_prec) {
    (ExpPrec::Infix(prec), ExpPrec::Infix(min_prec)) => should_break(prec, Some(min_prec)),
    (ExpPrec::Andalso, ExpPrec::Andalso) | (ExpPrec::Orelse, ExpPrec::Orelse) => ShouldBreak::Yes,
    (ExpPrec::Infix(_) | ExpPrec::Andalso | ExpPrec::Orelse, _) => ShouldBreak::No,
    (ExpPrec::Min, _) => unreachable!(),
  }
}
