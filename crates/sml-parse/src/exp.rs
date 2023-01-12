//! Parsing expressions.

use crate::parser::{ErrorKind, Exited, Expected, ParensExpFlavor, Parser};
use crate::util::{
  comma_sep, end_sep, lab, many_sep, must, name_star_eq, path_infix, path_no_infix, scon,
  should_break, InfixErr,
};
use crate::{dec::dec, pat::pat, ty::ty};
use sml_syntax::SyntaxKind as SK;

/// if no parse, emit error
pub(crate) fn exp(p: &mut Parser<'_>) -> bool {
  must(p, |p| exp_prec(p, ExpPrec::Min), Expected::Exp)
}

/// if no parse, do nothing
pub(crate) fn exp_opt(p: &mut Parser<'_>) -> bool {
  exp_prec(p, ExpPrec::Min).is_some()
}

/// if no parse, do nothing
pub(crate) fn eq_exp(p: &mut Parser<'_>) {
  if p.at(SK::Eq) {
    let en = p.enter();
    p.bump();
    exp(p);
    p.exit(en, SK::EqExp);
  }
}

#[allow(clippy::too_many_lines)]
fn exp_prec(p: &mut Parser<'_>, min_prec: ExpPrec) -> Option<Exited> {
  let en = p.enter();
  let infix = matches!(min_prec, ExpPrec::Infix(_));
  let ex = if p.at(SK::RaiseKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Raise));
    }
    p.bump();
    exp(p);
    p.exit(en, SK::RaiseExp)
  } else if p.at(SK::IfKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::If));
    }
    p.bump();
    exp(p);
    p.eat(SK::ThenKw);
    exp(p);
    p.eat(SK::ElseKw);
    exp(p);
    p.exit(en, SK::IfExp)
  } else if p.at(SK::WhileKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::While));
    }
    p.bump();
    exp(p);
    p.eat(SK::DoKw);
    exp(p);
    p.exit(en, SK::WhileExp)
  } else if p.at(SK::CaseKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Case));
    }
    p.bump();
    exp(p);
    p.eat(SK::OfKw);
    matcher(p);
    p.exit(en, SK::CaseExp)
  } else if p.at(SK::FnKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Fn));
    }
    p.bump();
    matcher(p);
    p.exit(en, SK::FnExp)
  } else {
    p.abandon(en);
    let mut ex = at_exp(p)?;
    loop {
      let op_info = if name_star_eq(p) {
        let text = p.peek().unwrap().text;
        p.get_infix(text)
      } else {
        None
      };
      ex = if let Some(op_info) = op_info {
        if should_break_exp(p, ExpPrec::Infix(op_info), min_prec) {
          break;
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
        if should_break_exp(p, ExpPrec::Andalso, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        must(p, |p| exp_prec(p, ExpPrec::Andalso), Expected::Exp);
        p.exit(en, SK::AndalsoExp)
      } else if p.at(SK::OrelseKw) {
        if should_break_exp(p, ExpPrec::Orelse, min_prec) {
          break;
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

/// when adding more cases to this, update [`at_exp_hd`].
fn at_exp(p: &mut Parser<'_>) -> Option<Exited> {
  let en = p.enter();
  let ex = if p.at(SK::DotDotDot) {
    p.bump();
    p.exit(en, SK::HoleExp)
  } else if p.at(SK::Underscore) {
    p.bump();
    p.exit(en, SK::WildcardExp)
  } else if scon(p) {
    p.bump();
    p.exit(en, SK::SConExp)
  } else if p.at(SK::OpKw) {
    if p.at_n(1, SK::AndalsoKw) {
      p.bump();
      p.bump();
      p.exit(en, SK::OpAndalsoExp)
    } else if p.at_n(1, SK::OrelseKw) {
      p.bump();
      p.bump();
      p.exit(en, SK::OpOrelseExp)
    } else {
      path_infix(p);
      p.exit(en, SK::PathExp)
    }
  } else if name_star_eq(p) {
    path_no_infix(p);
    p.exit(en, SK::PathExp)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::ExpRow, SK::RCurly, |p| {
      lab(p);
      eq_exp(p);
    });
    p.exit(en, SK::RecordExp)
  } else if p.at(SK::Hash) {
    p.bump();
    if p.at(SK::LSquare) {
      let list = p.enter();
      p.bump();
      exp_args(p);
      p.exit(list, SK::ListExp);
      p.exit(en, SK::VectorExp)
    } else {
      lab(p);
      p.exit(en, SK::SelectorExp)
    }
  } else if p.at(SK::LRound) {
    p.bump();
    let kind = at_exp_l_round(p);
    p.exit(en, kind)
  } else if p.at(SK::LSquare) {
    p.bump();
    exp_args(p);
    p.exit(en, SK::ListExp)
  } else if p.at(SK::LetKw) {
    p.bump();
    dec(p, InfixErr::Yes);
    p.eat(SK::InKw);
    end_sep(p, SK::ExpInSeq, SK::Semicolon, SK::EndKw, |p| {
      exp(p);
    });
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
    p.abandon(en);
    if p.at(SK::RRound) {
      p.bump();
    } else {
      p.error(ErrorKind::Expected(Expected::LRoundExpTail));
    }
    return SK::ParenExp;
  }
  let sep = p.bump().kind;
  let (wrap, overall) = match sep {
    SK::Semicolon => (SK::ExpInSeq, SK::SeqExp),
    SK::Comma => (SK::ExpArg, SK::TupleExp),
    _ => unreachable!("just checked at either ; or , above"),
  };
  p.exit(en, wrap);
  end_sep(p, wrap, sep, SK::RRound, |p| {
    exp(p);
  });
  overall
}

fn matcher(p: &mut Parser<'_>) {
  let en = p.enter();
  if p.at(SK::Bar) {
    p.bump();
  }
  many_sep(p, SK::Bar, SK::MatchRule, |p| {
    if !must(p, |p| pat(p, InfixErr::Yes), Expected::Pat) {
      return false;
    }
    p.eat(SK::EqGt);
    exp(p);
    true
  });
  p.exit(en, SK::Matcher);
}

/// see [`at_exp`]. need this for app expressions to know whether to precede or not.
fn at_exp_hd(p: &mut Parser<'_>) -> bool {
  p.at(SK::DotDotDot)
    || p.at(SK::Underscore)
    || scon(p)
    || p.at(SK::OpKw)
    || name_star_eq(p)
    || p.at(SK::LCurly)
    || p.at(SK::Hash)
    || p.at(SK::LRound)
    || p.at(SK::LSquare)
    || p.at(SK::LetKw)
}

fn exp_args(p: &mut Parser<'_>) {
  comma_sep(p, SK::ExpArg, SK::RSquare, |p| {
    exp(p);
  });
}

#[derive(Debug, Clone, Copy)]
enum ExpPrec {
  Min,
  Orelse,
  Andalso,
  Infix(sml_fixity::Infix),
}

fn should_break_exp(p: &mut Parser<'_>, prec: ExpPrec, min_prec: ExpPrec) -> bool {
  match (prec, min_prec) {
    (ExpPrec::Infix(prec), ExpPrec::Infix(min_prec)) => should_break(p, prec, min_prec),
    (_, ExpPrec::Min) | (ExpPrec::Infix(_), _) | (ExpPrec::Andalso, ExpPrec::Orelse) => false,
    (_, ExpPrec::Infix(_))
    | (ExpPrec::Andalso, ExpPrec::Andalso)
    | (ExpPrec::Orelse, ExpPrec::Orelse | ExpPrec::Andalso) => true,
    (ExpPrec::Min, _) => unreachable!("Min is only ever the starting prec, not a new prec"),
  }
}
