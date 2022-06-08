use crate::parser::{Assoc, ErrorKind, Exited, Expected, OpInfo, Parser};
use syntax::{token::Token, SyntaxKind as SK};

pub(crate) fn must<'a, F>(p: &mut Parser<'a>, f: F, e: Expected)
where
  F: FnOnce(&mut Parser<'a>) -> Option<Exited>,
{
  if f(p).is_none() {
    p.error(ErrorKind::Expected(e));
  }
}

/// similar to `many_sep`, but:
/// - always uses `;` as the separator
/// - allows the separator to not be present
/// - in return, `f` must say whether it parsed anything
/// returns whether it advanced at all.
pub(crate) fn maybe_semi_sep<'a, F>(p: &mut Parser<'a>, wrap: SK, mut f: F) -> bool
where
  F: FnMut(&mut Parser<'a>) -> Option<Exited>,
{
  let mut advanced = false;
  loop {
    let en = p.enter();
    if f(p).is_none() {
      p.abandon(en);
      return advanced;
    }
    advanced = true;
    if p.at(SK::Semicolon) {
      p.bump();
    }
    p.exit(en, wrap);
  }
}

/// similar to `many_sep`, but:
///
/// - always uses `,` as the separator
/// - allows 0 occurrences of `f`
/// - returns only after eating `end`
pub(crate) fn comma_sep<'a, F>(p: &mut Parser<'a>, end: SK, wrap: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a>),
{
  if p.at(end) {
    p.bump();
    return;
  }
  loop {
    let en = p.enter();
    f(p);
    if p.at(SK::Comma) {
      p.bump();
      p.exit(en, wrap);
    } else {
      p.exit(en, wrap);
      p.eat(end);
      break;
    }
  }
}

/// tries `f` at least once. stops if `sep` is not found. `wrap` will wrap both `f` and `sep` if
/// present.
pub(crate) fn many_sep<'a, F>(p: &mut Parser<'a>, sep: SK, wrap: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a>),
{
  loop {
    let en = p.enter();
    f(p);
    if p.at(sep) {
      p.bump();
      p.exit(en, wrap);
    } else {
      p.exit(en, wrap);
      break;
    }
  }
}

pub(crate) enum ShouldBreak {
  Yes,
  No,
  Error,
}

pub(crate) fn should_break(op_info: OpInfo, min_prec: Option<OpInfo>) -> ShouldBreak {
  match min_prec {
    None => ShouldBreak::No,
    Some(min_prec) => {
      if op_info.num == min_prec.num && op_info.assoc != min_prec.assoc {
        ShouldBreak::Error
      } else {
        let res = match min_prec.assoc {
          Assoc::Left => op_info.num <= min_prec.num,
          Assoc::Right => op_info.num < min_prec.num,
        };
        if res {
          ShouldBreak::Yes
        } else {
          ShouldBreak::No
        }
      }
    }
  }
}

#[must_use]
pub(crate) fn path(p: &mut Parser<'_>) -> Option<Exited> {
  if !name_star_eq(p) {
    return None;
  }
  let en = p.enter();
  let mut np_dot = p.enter();
  p.bump();
  loop {
    if p.at(SK::Dot) {
      p.bump();
      p.exit(np_dot, SK::NameStarEqDot);
      np_dot = p.enter();
      eat_name_star(p);
    } else {
      p.exit(np_dot, SK::NameStarEqDot);
      break;
    }
  }
  Some(p.exit(en, SK::Path))
}

/// requires we just got a true `name_star_eq(p)`. errors if this parses a path with no structures (aka
/// just a name) and that name is infix.
pub(crate) fn path_no_infix(p: &mut Parser<'_>) {
  let cur = p.peek().unwrap();
  if !p.at_n(1, SK::Dot) && p.contains_op(cur.text) {
    p.error(ErrorKind::InfixWithoutOp);
    let en = p.enter();
    p.eat(SK::Name);
    p.exit(en, SK::Path);
  } else {
    must(p, path, Expected::Path)
  }
}

#[must_use]
pub(crate) fn scon(p: &mut Parser<'_>) -> bool {
  p.at(SK::IntLit)
    || p.at(SK::RealLit)
    || p.at(SK::WordLit)
    || p.at(SK::CharLit)
    || p.at(SK::StringLit)
}

pub(crate) fn lab(p: &mut Parser<'_>) {
  if p.at(SK::Name) || p.at(SK::Star) || p.at(SK::IntLit) {
    p.bump();
  } else {
    p.error(ErrorKind::Expected(Expected::Lab));
  }
}

/// kind of badly named. it means Name, * or =
#[must_use]
pub(crate) fn name_star_eq(p: &mut Parser<'_>) -> bool {
  name_star(p, 0) || p.at(SK::Eq)
}

/// kind of badly named. it means Name or *
#[must_use]
pub(crate) fn name_star(p: &mut Parser<'_>, n: usize) -> bool {
  p.at_n(n, SK::Name) || p.at_n(n, SK::Star)
}

/// see [`name_star`].
pub(crate) fn eat_name_star<'a>(p: &mut Parser<'a>) -> Option<Token<'a, SK>> {
  if name_star(p, 0) {
    Some(p.bump())
  } else {
    p.error(ErrorKind::ExpectedKind(SK::Name));
    None
  }
}
