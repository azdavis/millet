use crate::parser::{Assoc, ErrorKind, Exited, OpInfo, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn must<'a, F>(p: &mut Parser<'a>, f: F)
where
  F: FnOnce(&mut Parser<'a>) -> Option<Exited>,
{
  if f(p).is_none() {
    p.error();
  }
}

/// similar to `many_sep`, but:
/// - always uses `;` as the separator
/// - allows the separator to not be present
/// - in return, `f` must say whether it parsed anything
pub(crate) fn maybe_semi_sep<'a, F>(p: &mut Parser<'a>, wrap: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a>) -> Option<Exited>,
{
  loop {
    let ent = p.enter();
    if f(p).is_none() {
      p.abandon(ent);
      break;
    }
    if p.at(SK::Semicolon) {
      p.bump();
    }
    p.exit(ent, wrap);
  }
}

/// stops if the sep is not found
pub(crate) fn many_sep<'a, F>(p: &mut Parser<'a>, sep: SK, wrap: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a>),
{
  loop {
    let ent = p.enter();
    f(p);
    if p.at(sep) {
      p.bump();
      p.exit(ent, wrap);
    } else {
      p.exit(ent, wrap);
      break;
    }
  }
}

pub(crate) fn should_break(p: &mut Parser<'_>, op_info: OpInfo, min_prec: Option<OpInfo>) -> bool {
  match min_prec {
    None => false,
    Some(min_prec) => {
      if op_info.num == min_prec.num && op_info.assoc != min_prec.assoc {
        p.error_with(ErrorKind::SameFixityDiffAssoc)
      }
      match min_prec.assoc {
        Assoc::Left => op_info.num <= min_prec.num,
        Assoc::Right => op_info.num < min_prec.num,
      }
    }
  }
}

#[must_use]
pub(crate) fn path(p: &mut Parser<'_>) -> Option<Exited> {
  if !p.at(SK::Name) {
    return None;
  }
  let e = p.enter();
  p.bump();
  loop {
    if p.at(SK::Dot) {
      p.bump();
      p.eat(SK::Name);
    } else {
      break;
    }
  }
  Some(p.exit(e, SK::Path))
}

#[must_use]
pub(crate) fn scon(p: &mut Parser<'_>) -> bool {
  if p.at(SK::IntLit)
    || p.at(SK::RealLit)
    || p.at(SK::WordLit)
    || p.at(SK::CharLit)
    || p.at(SK::StringLit)
  {
    p.bump();
    true
  } else {
    false
  }
}

#[must_use]
pub(crate) fn lab(p: &mut Parser<'_>) -> Option<Exited> {
  let ent = p.enter();
  let ex = if p.at(SK::Name) {
    p.bump();
    p.exit(ent, SK::NameLab)
  } else if p.at(SK::IntLit) {
    p.bump();
    p.exit(ent, SK::IntLitLab)
  } else {
    p.abandon(ent);
    return None;
  };
  Some(ex)
}
