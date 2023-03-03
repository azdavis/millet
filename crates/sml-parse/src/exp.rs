//! Parsing expressions.

use crate::parser::{ErrorKind, Exited, Expected, ParensExpFlavor, Parser};
use crate::util::{
  comma_sep, end_sep, lab, many_sep, name_star_eq, path_infix, path_no_infix, scon, should_break,
  InfixErr,
};
use crate::{dec::dec, pat::pat, ty::ty};
use sml_syntax::SyntaxKind as SK;

/// if no parse, emit error
pub(crate) fn exp(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> bool {
  let got = exp_prec(p, fe, ExpPrec::Min).is_some();
  if !got {
    p.error(ErrorKind::Expected(Expected::Exp));
  }
  got
}

/// if no parse, do nothing
pub(crate) fn exp_opt(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> bool {
  exp_prec(p, fe, ExpPrec::Min).is_some()
}

/// if no parse, do nothing
pub(crate) fn eq_exp(p: &mut Parser<'_>, fe: &sml_fixity::Env) {
  if p.at(SK::Eq) {
    let en = p.enter();
    p.bump();
    exp(p, fe);
    p.exit(en, SK::EqExp);
  }
}

fn exp_prec(p: &mut Parser<'_>, fe: &sml_fixity::Env, min_prec: ExpPrec) -> Option<Exited> {
  let en = p.enter();
  let infix = matches!(min_prec, ExpPrec::Infix(_));
  let ex = if p.at(SK::RaiseKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Raise));
    }
    p.bump();
    exp(p, fe);
    p.exit(en, SK::RaiseExp)
  } else if p.at(SK::IfKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::If));
    }
    p.bump();
    exp(p, fe);
    p.eat(SK::ThenKw);
    exp(p, fe);
    p.eat(SK::ElseKw);
    exp(p, fe);
    p.exit(en, SK::IfExp)
  } else if p.at(SK::WhileKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::While));
    }
    p.bump();
    exp(p, fe);
    p.eat(SK::DoKw);
    exp(p, fe);
    p.exit(en, SK::WhileExp)
  } else if p.at(SK::CaseKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Case));
    }
    p.bump();
    exp(p, fe);
    p.eat(SK::OfKw);
    matcher(p, fe);
    p.exit(en, SK::CaseExp)
  } else if p.at(SK::FnKw) {
    if infix {
      p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Fn));
    }
    p.bump();
    matcher(p, fe);
    p.exit(en, SK::FnExp)
  } else {
    p.abandon(en);
    let mut ex = at_exp(p, fe)?;
    loop {
      let op_info = if name_star_eq(p) {
        let text = p.peek().unwrap().text;
        fe.get(text).copied()
      } else {
        None
      };
      ex = if let Some(op_info) = op_info {
        if should_break_exp(p, ExpPrec::Infix(op_info), min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        if exp_prec(p, fe, ExpPrec::Infix(op_info)).is_none() {
          p.error(ErrorKind::Expected(Expected::Exp));
        }
        p.exit(en, SK::InfixExp)
      } else if p.at(SK::Colon) {
        if should_break_exp(p, ExpPrec::Colon, min_prec) {
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
        if exp_prec(p, fe, ExpPrec::Andalso).is_none() {
          p.error(ErrorKind::Expected(Expected::Exp));
        }
        p.exit(en, SK::AndalsoExp)
      } else if p.at(SK::OrelseKw) {
        if should_break_exp(p, ExpPrec::Orelse, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        if exp_prec(p, fe, ExpPrec::Orelse).is_none() {
          p.error(ErrorKind::Expected(Expected::Exp));
        }
        p.exit(en, SK::OrelseExp)
      } else if p.at(SK::HandleKw) {
        if should_break_exp(p, ExpPrec::Handle, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        matcher(p, fe);
        p.exit(en, SK::HandleExp)
      } else if at_exp_hd(p) {
        let en = p.precede(ex);
        if at_exp(p, fe).is_none() {
          p.error(ErrorKind::Expected(Expected::Exp));
        }
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
fn at_exp(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> Option<Exited> {
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
      path_infix(p, fe);
      p.exit(en, SK::PathExp)
    }
  } else if name_star_eq(p) {
    path_no_infix(p, fe);
    p.exit(en, SK::PathExp)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::ExpRow, SK::RCurly, |p| {
      lab(p);
      eq_exp(p, fe);
    });
    p.exit(en, SK::RecordExp)
  } else if p.at(SK::Hash) {
    p.bump();
    if p.at(SK::LSquare) {
      let list = p.enter();
      p.bump();
      exp_args(p, fe);
      p.exit(list, SK::ListExp);
      p.exit(en, SK::VectorExp)
    } else {
      lab(p);
      p.exit(en, SK::SelectorExp)
    }
  } else if p.at(SK::LRound) {
    p.bump();
    let kind = at_exp_l_round(p, fe);
    p.exit(en, kind)
  } else if p.at(SK::LSquare) {
    p.bump();
    exp_args(p, fe);
    p.exit(en, SK::ListExp)
  } else if p.at(SK::LetKw) {
    p.bump();
    let mut fe = fe.clone();
    dec(p, &mut fe, InfixErr::Yes);
    p.eat(SK::InKw);
    end_sep(p, SK::ExpInSeq, SK::Semicolon, SK::EndKw, |p| {
      exp(p, &fe);
    });
    p.exit(en, SK::LetExp)
  } else {
    p.abandon(en);
    return None;
  };
  Some(ex)
}

fn at_exp_l_round(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> SK {
  if p.at(SK::RRound) {
    p.bump();
    return SK::TupleExp;
  }
  let en = p.enter();
  exp(p, fe);
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
    exp(p, fe);
  });
  overall
}

fn matcher(p: &mut Parser<'_>, fe: &sml_fixity::Env) {
  let en = p.enter();
  if p.at(SK::Bar) {
    p.bump();
  }
  many_sep(p, SK::Bar, SK::MatchRule, |p| {
    if pat(p, fe, InfixErr::Yes).is_none() {
      p.error(ErrorKind::Expected(Expected::Pat));
      return false;
    }
    p.eat(SK::EqGt);
    exp(p, fe);
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

fn exp_args(p: &mut Parser<'_>, fe: &sml_fixity::Env) {
  comma_sep(p, SK::ExpArg, SK::RSquare, |p| {
    exp(p, fe);
  });
}

#[derive(Debug, Clone, Copy)]
enum ExpPrec {
  Min,
  Handle,
  Orelse,
  Andalso,
  Colon,
  Infix(sml_fixity::Infix),
}

fn should_break_exp(p: &mut Parser<'_>, prec: ExpPrec, min_prec: ExpPrec) -> bool {
  match (prec, min_prec) {
    (_, ExpPrec::Handle | ExpPrec::Colon) => unreachable!("Handle and Colon are never a min_prec"),
    (ExpPrec::Min, _) => unreachable!("Min is always a min_prec"),
    (ExpPrec::Infix(prec), ExpPrec::Infix(min_prec)) => should_break(p, prec, min_prec),
    (_, ExpPrec::Min)
    | (ExpPrec::Infix(_), _)
    | (ExpPrec::Colon, ExpPrec::Andalso | ExpPrec::Orelse)
    | (ExpPrec::Andalso, ExpPrec::Orelse) => false,
    (_, ExpPrec::Infix(_))
    | (ExpPrec::Andalso, ExpPrec::Andalso)
    | (ExpPrec::Orelse | ExpPrec::Handle, ExpPrec::Andalso | ExpPrec::Orelse) => true,
  }
}
