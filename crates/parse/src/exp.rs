use crate::dec::dec;
use crate::parser::{Exited, Parser};
use crate::util::{comma_sep, lab, many_sep, must, path, scon};
use syntax::SyntaxKind as SK;

pub(crate) fn exp(p: &mut Parser<'_>) -> Exited {
  todo!()
}

#[must_use]
fn at_exp(p: &mut Parser<'_>) -> Option<Exited> {
  let ent = p.enter();
  let ex = if scon(p) {
    p.exit(ent, SK::SConExp)
  } else if p.at(SK::OpKw) {
    p.bump();
    must(p, path);
    p.exit(ent, SK::PathExp)
  } else if p.at(SK::Name) {
    must(p, path);
    p.exit(ent, SK::PathExp)
  } else if p.at(SK::LCurly) {
    p.bump();
    comma_sep(p, SK::RCurly, SK::ExpRow, |p| {
      must(p, lab);
      p.eat(SK::Eq);
      exp(p);
    });
    p.exit(ent, SK::RecordExp)
  } else if p.at(SK::Hash) {
    p.bump();
    must(p, lab);
    p.exit(ent, SK::SelectorExp)
  } else if p.at(SK::LRound) {
    p.bump();
    comma_sep(p, SK::Comma, SK::ExpArg, |p| {
      exp(p);
    });
    p.eat(SK::RRound);
    p.exit(ent, SK::TupleExp)
  } else if p.at(SK::LSquare) {
    p.bump();
    comma_sep(p, SK::Comma, SK::ExpArg, |p| {
      exp(p);
    });
    p.eat(SK::RSquare);
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
