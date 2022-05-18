use crate::parser::{Entered, ErrorKind, Exited, Expected, OpInfo, Parser};
use crate::ty::{ty, ty_annotation};
use crate::util::{comma_sep, lab, must, path, scon, should_break, ShouldBreak};
use syntax::SyntaxKind as SK;

#[must_use]
pub(crate) fn pat(p: &mut Parser<'_>) -> Option<Exited> {
  pat_prec(p, None)
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
  ConPatArg(Entered),
  Infix(ConPatState, OpInfo),
}

/// kind of gross for the tricky ones (as pat, con pat with arg, infix pat).
#[must_use]
fn pat_prec(p: &mut Parser<'_>, min_prec: Option<OpInfo>) -> Option<Exited> {
  // as pat/typed pat
  if name_then_colon_or_as(p, 0) || (p.at(SK::OpKw) && name_then_colon_or_as(p, 1)) {
    let en = p.enter();
    if p.at(SK::OpKw) {
      p.bump();
    }
    p.eat(SK::Name);
    let ta = ty_annotation(p);
    let ap = as_pat_tl(p);
    let kind = match (ta, ap) {
      (_, Some(_)) => SK::AsPat,
      (Some(_), None) => SK::TypedNamePat,
      (None, None) => unreachable!("we should have just seen a name, then either colon or as"),
    };
    return Some(p.exit(en, kind));
  }
  // con pat with arg, or infix pat
  let mut state = if p.at(SK::Name) || (p.at(SK::OpKw) && p.at_n(1, SK::Name)) {
    let en = p.enter();
    if p.at(SK::OpKw) {
      p.bump();
    }
    must(p, path, Expected::Path);
    ConPatState::Entered(en)
  } else {
    ConPatState::Exited(at_pat(p)?)
  };
  loop {
    let ex = if at_pat_hd(p) {
      let tok = p.peek().unwrap();
      let at_pat_hd = if tok.kind == SK::Name {
        match p.get_op(tok.text) {
          None => match state {
            ConPatState::Entered(en) => AtPatHd::ConPatArg(en),
            ConPatState::Exited(_) => {
              p.error(ErrorKind::NotInfix);
              AtPatHd::Infix(state, OpInfo::left(0))
            }
          },
          Some(op_info) => AtPatHd::Infix(state, op_info),
        }
      } else {
        match state {
          ConPatState::Entered(en) => AtPatHd::ConPatArg(en),
          ConPatState::Exited(_) => break,
        }
      };
      match at_pat_hd {
        AtPatHd::ConPatArg(en) => {
          must(p, at_pat, Expected::Pat);
          p.exit(en, SK::ConPat)
        }
        AtPatHd::Infix(st, op_info) => {
          state = st;
          match should_break(op_info, min_prec) {
            ShouldBreak::Yes => break,
            ShouldBreak::No => {}
            ShouldBreak::Error => p.error(ErrorKind::SameFixityDiffAssoc),
          }
          let ex = state.exit(p);
          let en = p.precede(ex);
          p.bump();
          must(p, |p| pat_prec(p, Some(op_info)), Expected::Pat);
          p.exit(en, SK::InfixPat)
        }
      }
    } else if p.at(SK::Colon) {
      if min_prec.is_some() {
        break;
      }
      let ex = state.exit(p);
      let en = p.precede(ex);
      p.bump();
      ty(p);
      p.exit(en, SK::TypedPat)
    } else {
      break;
    };
    state = ConPatState::Exited(ex);
  }
  Some(state.exit(p))
}

/// when adding more cases to this, update [`at_pat_hd`]
#[must_use]
pub(crate) fn at_pat(p: &mut Parser<'_>) -> Option<Exited> {
  let en = p.enter();
  let ex = if scon(p) {
    p.bump();
    p.exit(en, SK::SConPat)
  } else if p.at(SK::Underscore) {
    p.bump();
    p.exit(en, SK::WildcardPat)
  } else if p.at(SK::OpKw) {
    p.bump();
    must(p, path, Expected::Path);
    p.exit(en, SK::ConPat)
  } else if p.at(SK::Name) {
    must(p, path, Expected::Path);
    p.exit(en, SK::ConPat)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::PatRow, |p| {
      let en = p.enter();
      if p.at(SK::DotDotDot) {
        p.bump();
        p.exit(en, SK::RestPatRow);
      } else if p.at_n(1, SK::Eq) {
        must(p, lab, Expected::Lab);
        p.eat(SK::Eq);
        must(p, pat, Expected::Pat);
        p.exit(en, SK::LabAndPatPatRow);
      } else {
        p.eat(SK::Name);
        let _ = ty_annotation(p);
        let _ = as_pat_tl(p);
        p.exit(en, SK::LabPatRow);
      }
    });
    p.exit(en, SK::RecordPat)
  } else if p.at(SK::LRound) {
    p.bump();
    let kind = at_pat_l_round(p);
    p.exit(en, kind)
  } else if p.at(SK::LSquare) {
    p.bump();
    comma_sep(p, SK::RSquare, SK::PatArg, |p| must(p, pat, Expected::Pat));
    p.exit(en, SK::ListPat)
  } else {
    p.abandon(en);
    return None;
  };
  Some(ex)
}

fn at_pat_hd(p: &mut Parser<'_>) -> bool {
  scon(p)
    || p.at(SK::Underscore)
    || p.at(SK::OpKw)
    || p.at(SK::Name)
    || p.at(SK::LCurly)
    || p.at(SK::LRound)
    || p.at(SK::LSquare)
}

fn at_pat_l_round(p: &mut Parser<'_>) -> SK {
  if p.at(SK::RRound) {
    p.bump();
    return SK::TuplePat;
  }
  let en = p.enter();
  must(p, pat, Expected::Pat);
  if p.at(SK::RRound) {
    p.abandon(en);
    p.bump();
    return SK::ParenPat;
  }
  p.eat(SK::Comma);
  p.exit(en, SK::PatArg);
  loop {
    let en = p.enter();
    must(p, pat, Expected::Pat);
    if p.at(SK::Comma) {
      p.bump();
      p.exit(en, SK::PatArg);
    } else {
      p.exit(en, SK::PatArg);
      p.eat(SK::RRound);
      break;
    }
  }
  SK::TuplePat
}

/// not necessarily an as pat, since it could be a typed pat.
#[must_use]
fn name_then_colon_or_as(p: &mut Parser<'_>, n: usize) -> bool {
  p.at_n(n, SK::Name) && (p.at_n(n + 1, SK::Colon) || p.at_n(n + 1, SK::AsKw))
}

#[must_use]
fn as_pat_tl(p: &mut Parser<'_>) -> Option<Exited> {
  if p.at(SK::AsKw) {
    let en = p.enter();
    p.bump();
    must(p, pat, Expected::Pat);
    Some(p.exit(en, SK::AsPatTail))
  } else {
    None
  }
}
