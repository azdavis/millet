use crate::parser::{Exited, Parser};
use crate::ty::ty_annotation;
use crate::util::{lab, many_sep, must, path, scon};
use syntax::SyntaxKind as SK;

#[must_use]
pub(crate) fn pat(p: &mut Parser<'_>) -> Option<Exited> {
  // first try the most annoying one
  let ent = p.enter();
  let save = p.save();
  if p.at(SK::OpKw) {
    p.bump();
  }
  p.eat(SK::Name);
  let _ = ty_annotation(p);
  must(p, as_pat_tail);
  if !p.error_since(&save) {
    return Some(p.exit(ent, SK::AsPat));
  }
  // then try the second-most annoying one
  p.restore(save);
  if p.at(SK::OpKw) && p.peek_n(1).map_or(false, |tok| tok.kind == SK::Name) {
    p.bump();
  }
  must(p, path);
  todo!()
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
    many_sep(p, SK::Comma, SK::PatRow, |p| {
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
    p.eat(SK::RCurly);
    p.exit(ent, SK::RecordPat)
  } else if p.at(SK::LRound) {
    p.bump();
    many_sep(p, SK::Comma, SK::PatArg, |p| must(p, pat));
    p.eat(SK::RRound);
    p.exit(ent, SK::TuplePat)
  } else if p.at(SK::LSquare) {
    p.bump();
    many_sep(p, SK::Comma, SK::PatArg, |p| must(p, pat));
    p.eat(SK::RSquare);
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
