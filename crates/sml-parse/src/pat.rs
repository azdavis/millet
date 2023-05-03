//! Parsing patterns.

use crate::parser::{Entered, ErrorKind, Exited, Expected, Parser};
use crate::ty::{ty, ty_annotation};
use crate::util::{
  comma_sep, eat_name_star, lab, name_star, path, path_infix, path_no_infix, should_break, InfixErr,
};
use sml_syntax::SyntaxKind as SK;

pub(crate) fn pat(p: &mut Parser<'_>, fe: &sml_fixity::Env, infix: InfixErr) -> Option<Exited> {
  pat_prec(p, fe, PatPrec::Min, infix)
}

enum ConPatState {
  /// we got the head of a con pat, but we're looking for the arg.
  Entered(Entered),
  /// we know we either got the arg or didn't. so we're not parsing a con pat right now.
  Exited(Exited),
}

impl ConPatState {
  fn exit(self, p: &mut Parser<'_>) -> Exited {
    match self {
      ConPatState::Entered(en) => p.exit(en, SK::ConPat),
      ConPatState::Exited(ex) => ex,
    }
  }
}

enum AtPatHd {
  /// corresponds to `ConPatState::Entered`.
  ConPatArg(Entered),
  /// we're parsing an infix pat.
  Infix(ConPatState, sml_fixity::Infix),
}

/// kind of gross for the tricky ones.
fn pat_prec(
  p: &mut Parser<'_>,
  fe: &sml_fixity::Env,
  min_prec: PatPrec,
  infix: InfixErr,
) -> Option<Exited> {
  // con pat with arg, or infix pat
  let mut state = if name_star(p, 0) || (p.at(SK::OpKw) && name_star(p, 1)) {
    let en = p.enter();
    if p.at(SK::OpKw) {
      path_infix(p, fe);
    } else {
      match infix {
        InfixErr::No => {
          path(p);
        }
        InfixErr::Yes => path_no_infix(p, fe),
      }
    }
    ConPatState::Entered(en)
  } else {
    ConPatState::Exited(at_pat(p, fe, infix)?)
  };
  while let Some(tok) = p.peek() {
    let ex = match tok.kind {
      SK::IntLit
      | SK::RealLit
      | SK::WordLit
      | SK::CharLit
      | SK::StringLit
      | SK::Underscore
      | SK::OpKw
      | SK::Name
      | SK::Star
      | SK::LCurly
      | SK::LRound
      | SK::LSquare
      | SK::Hash => {
        let at_pat_hd = if name_star(p, 0) {
          match fe.get(tok.text) {
            None => match state {
              ConPatState::Entered(en) => AtPatHd::ConPatArg(en),
              ConPatState::Exited(_) => {
                p.error(ErrorKind::NotInfix);
                AtPatHd::Infix(state, sml_fixity::Infix::left(0))
              }
            },
            Some(&op_info) => AtPatHd::Infix(state, op_info),
          }
        } else {
          match state {
            ConPatState::Entered(en) => AtPatHd::ConPatArg(en),
            ConPatState::Exited(_) => break,
          }
        };
        match at_pat_hd {
          AtPatHd::ConPatArg(en) => {
            if at_pat(p, fe, infix).is_none() {
              p.error(ErrorKind::Expected(Expected::Pat));
            }
            p.exit(en, SK::ConPat)
          }
          AtPatHd::Infix(st, op_info) => {
            state = st;
            match min_prec {
              PatPrec::Min | PatPrec::Or => {}
              PatPrec::Infix(min_prec) => {
                if should_break(p, op_info, min_prec) {
                  break;
                }
              }
            }
            let ex = state.exit(p);
            let en = p.precede(ex);
            p.bump();
            if pat_prec(p, fe, PatPrec::Infix(op_info), infix).is_none() {
              p.error(ErrorKind::Expected(Expected::Pat));
            }
            p.exit(en, SK::InfixPat)
          }
        }
      }
      SK::Bar => {
        match min_prec {
          PatPrec::Min => {}
          PatPrec::Or | PatPrec::Infix(_) => break,
        }
        let ex = state.exit(p);
        let en = p.precede(ex);
        p.bump();
        if pat_prec(p, fe, PatPrec::Or, infix).is_none() {
          p.error(ErrorKind::Expected(Expected::Pat));
        }
        p.exit(en, SK::OrPat)
      }
      SK::Colon | SK::ColonGt => {
        match min_prec {
          PatPrec::Min | PatPrec::Or => {}
          PatPrec::Infix(_) => break,
        }
        let ex = state.exit(p);
        let en = p.precede(ex);
        p.bump();
        ty(p);
        p.exit(en, SK::TypedPat)
      }
      SK::AsKw => {
        match min_prec {
          PatPrec::Min | PatPrec::Or => {}
          PatPrec::Infix(_) => break,
        }
        let ex = state.exit(p);
        let en = p.precede(ex);
        if as_pat_tl(p, fe, infix).is_none() {
          p.error(ErrorKind::Expected(Expected::Pat));
        }
        p.exit(en, SK::AsPat)
      }
      _ => break,
    };
    state = ConPatState::Exited(ex);
  }
  Some(state.exit(p))
}

#[derive(Debug, Clone, Copy)]
enum PatPrec {
  Min,
  Or,
  Infix(sml_fixity::Infix),
}

/// when adding more cases to this, update [`at_pat_hd`].
pub(crate) fn at_pat(p: &mut Parser<'_>, fe: &sml_fixity::Env, infix: InfixErr) -> Option<Exited> {
  let kind = p.peek()?.kind;
  let en = p.enter();
  let kind = match kind {
    SK::IntLit | SK::RealLit | SK::WordLit | SK::CharLit | SK::StringLit => {
      p.bump();
      SK::SConPat
    }
    SK::Underscore => {
      p.bump();
      SK::WildcardPat
    }
    SK::OpKw => {
      path_infix(p, fe);
      SK::ConPat
    }
    SK::Name | SK::Star => {
      match infix {
        InfixErr::No => {
          path(p);
        }
        InfixErr::Yes => path_no_infix(p, fe),
      }
      SK::ConPat
    }
    SK::LCurly => {
      p.bump();
      comma_sep(p, SK::PatRow, SK::RCurly, |p| {
        let en = p.enter();
        if p.at(SK::DotDotDot) {
          p.bump();
          p.exit(en, SK::RestPatRow);
        } else if p.at_n(1, SK::Eq) {
          lab(p);
          p.eat(SK::Eq);
          if pat(p, fe, infix).is_none() {
            p.error(ErrorKind::Expected(Expected::Pat));
          }
          p.exit(en, SK::LabAndPatPatRow);
        } else {
          eat_name_star(p);
          _ = ty_annotation(p);
          _ = as_pat_tl(p, fe, infix);
          p.exit(en, SK::LabPatRow);
        }
      });
      SK::RecordPat
    }
    SK::LRound => {
      p.bump();
      at_pat_l_round(p, fe, infix)
    }
    SK::LSquare => {
      p.bump();
      pat_args(p, fe, SK::RSquare, infix);
      SK::ListPat
    }
    SK::Hash => {
      p.bump();
      let list = p.enter();
      p.eat(SK::LSquare);
      pat_args(p, fe, SK::RSquare, infix);
      p.exit(list, SK::ListPat);
      SK::VectorPat
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  Some(p.exit(en, kind))
}

fn at_pat_l_round(p: &mut Parser<'_>, fe: &sml_fixity::Env, infix: InfixErr) -> SK {
  if p.at(SK::RRound) {
    p.bump();
    return SK::TuplePat;
  }
  let en = p.enter();
  if pat(p, fe, infix).is_none() {
    p.error(ErrorKind::Expected(Expected::Pat));
  }
  if p.at(SK::RRound) {
    p.abandon(en);
    p.bump();
    return SK::ParenPat;
  }
  p.eat(SK::Comma);
  p.exit(en, SK::PatArg);
  pat_args(p, fe, SK::RRound, infix);
  SK::TuplePat
}

fn as_pat_tl(p: &mut Parser<'_>, fe: &sml_fixity::Env, infix: InfixErr) -> Option<Exited> {
  if p.at(SK::AsKw) {
    let en = p.enter();
    p.bump();
    if pat(p, fe, infix).is_none() {
      p.error(ErrorKind::Expected(Expected::Pat));
    }
    Some(p.exit(en, SK::AsPatTail))
  } else {
    None
  }
}

fn pat_args(p: &mut Parser<'_>, fe: &sml_fixity::Env, end: SK, infix: InfixErr) {
  comma_sep(p, SK::PatArg, end, |p| {
    if pat(p, fe, infix).is_none() {
      p.error(ErrorKind::Expected(Expected::Pat));
    }
  });
}
