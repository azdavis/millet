use crate::parser::{ErrorKind, Exited, OpInfo, Parser};
use crate::ty::{ty, ty_annotation};
use crate::util::{comma_sep, lab, must, path, scon, should_break};
use syntax::SyntaxKind as SK;

#[must_use]
pub(crate) fn pat(p: &mut Parser<'_>) -> Option<Exited> {
  pat_prec(p, None)
}

#[must_use]
fn pat_prec(p: &mut Parser<'_>, min_prec: Option<OpInfo>) -> Option<Exited> {
  // first try AsPat since it's annoying
  let ent = p.enter();
  let save = p.save();
  if p.at(SK::OpKw) {
    p.bump();
  }
  p.eat(SK::Name);
  let _ = ty_annotation(p);
  must(p, as_pat_tail);
  if p.maybe_discard(save) {
    return Some(p.exit(ent, SK::AsPat));
  }
  // then try ConPat with arg
  if p.at(SK::OpKw) && p.peek_n(1).map_or(false, |tok| tok.kind == SK::Name) {
    p.bump();
  }
  let mut ex = if path(p).is_some() {
    let _ = at_pat(p);
    p.exit(ent, SK::ConPat)
  } else {
    // else, it's an atomic pat
    p.abandon(ent);
    at_pat(p)?
  };
  loop {
    if let Some(text) = p
      .peek()
      .and_then(|tok| (tok.kind == SK::Name).then(|| tok.text))
    {
      let op_info = match p.get_op(text) {
        Some(x) => x,
        None => {
          p.error_with(ErrorKind::NotInfix);
          // pretend it is
          OpInfo::left(0)
        }
      };
      if should_break(p, op_info, min_prec) {
        break;
      }
      let ent = p.precede(ex);
      p.bump();
      must(p, |p| pat_prec(p, Some(op_info)));
      ex = p.exit(ent, SK::InfixPat);
    } else if p.at(SK::Colon) {
      if min_prec.is_some() {
        break;
      }
      let ent = p.precede(ex);
      p.bump();
      ty(p);
      ex = p.exit(ent, SK::TypedPat);
    } else {
      break;
    }
  }
  Some(ex)
}

#[must_use]
pub(crate) fn at_pat(p: &mut Parser<'_>) -> Option<Exited> {
  let ent = p.enter();
  let ex = if p.at(SK::Underscore) {
    p.bump();
    p.exit(ent, SK::WildcardPat)
  } else if scon(p) {
    p.exit(ent, SK::SConPat)
  } else if p.at(SK::OpKw) {
    p.bump();
    must(p, path);
    p.exit(ent, SK::ConPat)
  } else if p.at(SK::Name) {
    must(p, path);
    p.exit(ent, SK::ConPat)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::PatRow, |p| {
      let ent = p.enter();
      if p.at(SK::DotDotDot) {
        p.bump();
        p.exit(ent, SK::RestPatRow);
      } else if p.peek_n(1).map_or(false, |tok| tok.kind == SK::Eq) {
        must(p, lab);
        p.eat(SK::Eq);
        must(p, pat);
        p.exit(ent, SK::LabAndPatPatRow);
      } else {
        p.eat(SK::Name);
        let _ = ty_annotation(p);
        let _ = as_pat_tail(p);
        p.exit(ent, SK::LabPatRow);
      }
    });
    p.exit(ent, SK::RecordPat)
  } else if p.at(SK::LRound) {
    p.bump();
    comma_sep(p, SK::RRound, SK::PatArg, |p| must(p, pat));
    p.exit(ent, SK::TuplePat)
  } else if p.at(SK::LSquare) {
    p.bump();
    comma_sep(p, SK::RSquare, SK::PatArg, |p| must(p, pat));
    p.exit(ent, SK::ListPat)
  } else {
    p.abandon(ent);
    return None;
  };
  Some(ex)
}

#[must_use]
fn as_pat_tail(p: &mut Parser<'_>) -> Option<Exited> {
  if p.at(SK::AsKw) {
    let ent = p.enter();
    p.bump();
    must(p, pat);
    Some(p.exit(ent, SK::AsPatTail))
  } else {
    None
  }
}
