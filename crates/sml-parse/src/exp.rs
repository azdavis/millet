//! Parsing expressions.

use crate::parser::{ErrorKind, Exited, Expected, ParensExpFlavor, Parser};
use crate::util::{
  comma_sep, end_sep, lab, many_sep, path_infix, path_no_infix, should_break, InfixErr,
};
use crate::{dec::dec, pat::pat, ty::ty};
use sml_syntax::kind::SyntaxKind as SK;

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
  let kind = p.peek()?.kind;
  let en = p.enter();
  let infix = matches!(min_prec, ExpPrec::Infix(_));
  let ex = match kind {
    SK::RaiseKw => {
      if infix {
        p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Raise));
      }
      p.bump();
      exp(p, fe);
      p.exit(en, SK::RaiseExp)
    }
    SK::IfKw => {
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
    }
    SK::WhileKw => {
      if infix {
        p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::While));
      }
      p.bump();
      exp(p, fe);
      p.eat(SK::DoKw);
      exp(p, fe);
      p.exit(en, SK::WhileExp)
    }
    SK::CaseKw => {
      if infix {
        p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Case));
      }
      p.bump();
      exp(p, fe);
      p.eat(SK::OfKw);
      matcher(p, fe);
      p.exit(en, SK::CaseExp)
    }
    SK::FnKw => {
      if infix {
        p.error(ErrorKind::NeedParensAroundExpHere(ParensExpFlavor::Fn));
      }
      p.bump();
      matcher(p, fe);
      p.exit(en, SK::FnExp)
    }
    _ => {
      p.abandon(en);
      let ex = at_exp(p, fe)?;
      exp_prec_loop(p, fe, min_prec, ex)
    }
  };
  Some(ex)
}

fn exp_prec_loop(
  p: &mut Parser<'_>,
  fe: &sml_fixity::Env,
  min_prec: ExpPrec,
  mut ex: Exited,
) -> Exited {
  while let Some(tok) = p.peek() {
    ex = match tok.kind {
      SK::Name | SK::Star | SK::Eq => match fe.get(tok.text) {
        Some(&op_info) => {
          if should_break_exp(p, ExpPrec::Infix(op_info), min_prec) {
            break;
          }
          let en = p.precede(ex);
          p.bump();
          if exp_prec(p, fe, ExpPrec::Infix(op_info)).is_none() {
            p.error(ErrorKind::Expected(Expected::Exp));
          }
          p.exit(en, SK::InfixExp)
        }
        None => app_exp(p, fe, ex),
      },
      SK::Colon | SK::ColonGt => {
        if should_break_exp(p, ExpPrec::Ascription, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        ty(p);
        p.exit(en, SK::TypedExp)
      }
      SK::AndalsoKw => {
        if should_break_exp(p, ExpPrec::Andalso, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        if exp_prec(p, fe, ExpPrec::Andalso).is_none() {
          p.error(ErrorKind::Expected(Expected::Exp));
        }
        p.exit(en, SK::AndalsoExp)
      }
      SK::OrelseKw => {
        if should_break_exp(p, ExpPrec::Orelse, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        if exp_prec(p, fe, ExpPrec::Orelse).is_none() {
          p.error(ErrorKind::Expected(Expected::Exp));
        }
        p.exit(en, SK::OrelseExp)
      }
      SK::HandleKw => {
        if should_break_exp(p, ExpPrec::Handle, min_prec) {
          break;
        }
        let en = p.precede(ex);
        p.bump();
        matcher(p, fe);
        p.exit(en, SK::HandleExp)
      }
      SK::DotDotDot
      | SK::Underscore
      | SK::IntLit
      | SK::RealLit
      | SK::WordLit
      | SK::CharLit
      | SK::StringLit
      | SK::OpKw
      | SK::LCurly
      | SK::Hash
      | SK::LRound
      | SK::LSquare
      | SK::LetKw => app_exp(p, fe, ex),
      _ => break,
    };
  }
  ex
}

fn app_exp(p: &mut Parser<'_>, fe: &sml_fixity::Env, ex: Exited) -> Exited {
  let en = p.precede(ex);
  if at_exp(p, fe).is_none() {
    p.error(ErrorKind::Expected(Expected::Exp));
  }
  p.exit(en, SK::AppExp)
}

fn at_exp(p: &mut Parser<'_>, fe: &sml_fixity::Env) -> Option<Exited> {
  let kind = p.peek()?.kind;
  let en = p.enter();
  let kind = match kind {
    SK::DotDotDot => {
      p.bump();
      SK::HoleExp
    }
    SK::Underscore => {
      p.bump();
      SK::WildcardExp
    }
    SK::IntLit | SK::RealLit | SK::WordLit | SK::CharLit | SK::StringLit => {
      p.bump();
      SK::SConExp
    }
    SK::OpKw => {
      if p.at_n(1, SK::AndalsoKw) {
        p.bump();
        p.bump();
        SK::OpAndalsoExp
      } else if p.at_n(1, SK::OrelseKw) {
        p.bump();
        p.bump();
        SK::OpOrelseExp
      } else {
        path_infix(p, fe);
        SK::PathExp
      }
    }
    SK::Name | SK::Star | SK::Eq => {
      path_no_infix(p, fe);
      SK::PathExp
    }
    SK::LCurly => {
      p.bump();
      comma_sep(p, SK::ExpRow, SK::RCurly, |p| {
        lab(p);
        eq_exp(p, fe);
      });
      SK::RecordExp
    }
    SK::Hash => {
      p.bump();
      if p.at(SK::LSquare) {
        let list = p.enter();
        p.bump();
        exp_args(p, fe);
        p.exit(list, SK::ListExp);
        SK::VectorExp
      } else {
        lab(p);
        SK::SelectorExp
      }
    }
    SK::LRound => {
      p.bump();
      at_exp_l_round(p, fe)
    }
    SK::LSquare => {
      p.bump();
      exp_args(p, fe);
      SK::ListExp
    }
    SK::LetKw => {
      p.bump();
      let mut fe = fe.clone();
      dec(p, &mut fe, InfixErr::Yes);
      p.eat(SK::InKw);
      end_sep(p, SK::ExpInSeq, SK::Semicolon, SK::EndKw, |p| {
        exp(p, &fe);
      });
      SK::LetExp
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  Some(p.exit(en, kind))
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
  many_sep(p, SK::Bar, SK::Arm, |p| {
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
  Ascription,
  Infix(sml_fixity::Infix),
}

fn should_break_exp(p: &mut Parser<'_>, prec: ExpPrec, min_prec: ExpPrec) -> bool {
  match (prec, min_prec) {
    (_, ExpPrec::Handle | ExpPrec::Ascription) => {
      unreachable!("Handle and Ascription are never a min_prec")
    }
    (ExpPrec::Min, _) => unreachable!("Min is always a min_prec"),
    (ExpPrec::Infix(prec), ExpPrec::Infix(min_prec)) => should_break(p, prec, min_prec),
    (_, ExpPrec::Min)
    | (ExpPrec::Infix(_), _)
    | (ExpPrec::Ascription, ExpPrec::Andalso | ExpPrec::Orelse)
    | (ExpPrec::Andalso, ExpPrec::Orelse) => false,
    (_, ExpPrec::Infix(_))
    | (ExpPrec::Andalso, ExpPrec::Andalso)
    | (ExpPrec::Orelse | ExpPrec::Handle, ExpPrec::Andalso | ExpPrec::Orelse) => true,
  }
}
