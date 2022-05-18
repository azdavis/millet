use crate::parser::{Entered, ErrorKind, Exited, Expected, OpInfo, Parser};
use crate::ty::{ty, ty_annotation};
use crate::util::{comma_sep, lab, must, path, scon, should_break};
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
      ConPatState::Entered(ent) => p.exit(ent, SK::ConPat),
      ConPatState::Exited(ex) => ex,
    }
  }
}

/// kind of gross for the tricky ones (as pat, con pat with arg, infix pat).
#[must_use]
fn pat_prec(p: &mut Parser<'_>, min_prec: Option<OpInfo>) -> Option<Exited> {
  // as pat
  if as_pat_hd(p, 0) || (p.at(SK::OpKw) && as_pat_hd(p, 1)) {
    let ent = p.enter();
    if p.at(SK::OpKw) {
      p.bump();
    }
    p.eat(SK::Name);
    let _ = ty_annotation(p);
    // using unwrap, not must, since we just checked for as_pat_hd.
    as_pat_tl(p).unwrap();
    return Some(p.exit(ent, SK::AsPat));
  }
  // con pat with arg, or infix pat
  let mut state = if p.at(SK::Name) || (p.at(SK::OpKw) && p.at_n(1, SK::Name)) {
    let ent = p.enter();
    if p.at(SK::OpKw) {
      p.bump();
    }
    must(p, path, Expected::Path);
    ConPatState::Entered(ent)
  } else {
    ConPatState::Exited(at_pat(p)?)
  };
  loop {
    let ex = if let Some(text) = p
      .peek()
      .and_then(|tok| (tok.kind == SK::Name).then(|| tok.text))
    {
      let op_info = match p.get_op(text) {
        Some(x) => x,
        None => {
          p.error(ErrorKind::NotInfix);
          // pretend it is
          OpInfo::left(0)
        }
      };
      if should_break(p, op_info, min_prec) {
        break;
      }
      let ex = state.exit(p);
      let ent = p.precede(ex);
      p.bump();
      must(p, |p| pat_prec(p, Some(op_info)), Expected::Pat);
      p.exit(ent, SK::InfixPat)
    } else if p.at(SK::Colon) {
      if min_prec.is_some() {
        break;
      }
      let ex = state.exit(p);
      let ent = p.precede(ex);
      p.bump();
      ty(p);
      p.exit(ent, SK::TypedPat)
    } else if at_pat_hd(p) {
      let ent = match state {
        ConPatState::Entered(ent) => ent,
        ConPatState::Exited(_) => break,
      };
      must(p, at_pat, Expected::Pat);
      p.exit(ent, SK::ConPat)
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
  let ent = p.enter();
  let ex = if scon(p) {
    p.bump();
    p.exit(ent, SK::SConPat)
  } else if p.at(SK::Underscore) {
    p.bump();
    p.exit(ent, SK::WildcardPat)
  } else if p.at(SK::OpKw) {
    p.bump();
    must(p, path, Expected::Path);
    p.exit(ent, SK::ConPat)
  } else if p.at(SK::Name) {
    must(p, path, Expected::Path);
    p.exit(ent, SK::ConPat)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::PatRow, |p| {
      let ent = p.enter();
      if p.at(SK::DotDotDot) {
        p.bump();
        p.exit(ent, SK::RestPatRow);
      } else if p.at_n(1, SK::Eq) {
        must(p, lab, Expected::Lab);
        p.eat(SK::Eq);
        must(p, pat, Expected::Pat);
        p.exit(ent, SK::LabAndPatPatRow);
      } else {
        p.eat(SK::Name);
        let _ = ty_annotation(p);
        let _ = as_pat_tl(p);
        p.exit(ent, SK::LabPatRow);
      }
    });
    p.exit(ent, SK::RecordPat)
  } else if p.at(SK::LRound) {
    p.bump();
    let one = comma_sep(p, SK::RRound, SK::PatArg, |p| must(p, pat, Expected::Pat));
    p.exit(ent, if one { SK::ParenPat } else { SK::TuplePat })
  } else if p.at(SK::LSquare) {
    p.bump();
    comma_sep(p, SK::RSquare, SK::PatArg, |p| must(p, pat, Expected::Pat));
    p.exit(ent, SK::ListPat)
  } else {
    p.abandon(ent);
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

#[must_use]
fn as_pat_hd(p: &mut Parser<'_>, n: usize) -> bool {
  p.at_n(n, SK::Name) && (p.at_n(n + 1, SK::Colon) || p.at_n(n + 1, SK::AsKw))
}

#[must_use]
fn as_pat_tl(p: &mut Parser<'_>) -> Option<Exited> {
  if p.at(SK::AsKw) {
    let ent = p.enter();
    p.bump();
    must(p, pat, Expected::Pat);
    Some(p.exit(ent, SK::AsPatTail))
  } else {
    None
  }
}
