use crate::parser::{Assoc, ErrorKind, Exited, Expected, Infix, Parser};
use sml_syntax::{token::Token, SyntaxKind as SK};

/// emits an error and returns false if `f` failed. otherwise returns `true`.
pub(crate) fn must<'a, F>(p: &mut Parser<'a>, f: F, e: Expected) -> bool
where
  F: FnOnce(&mut Parser<'a>) -> Option<Exited>,
{
  let ret = f(p).is_some();
  if !ret {
    p.error(ErrorKind::Expected(e));
  }
  ret
}

/// similar to `many_sep`, but:
/// - always uses `;` as the separator
/// - allows the separator to not be present
/// - in return, `f` must say whether it parsed anything
/// returns whether it advanced at all.
pub(crate) fn maybe_semi_sep<'a, F>(p: &mut Parser<'a>, wrap: SK, mut f: F) -> bool
where
  F: FnMut(&mut Parser<'a>) -> bool,
{
  let mut advanced = false;
  loop {
    let en = p.enter();
    if !f(p) {
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

/// if `f` returns false, it consumed nothing. (but it may consume nothing and return `true`.) stops
/// if `sep` is not found. `wrap` will wrap both `f` and `sep` if present.
///
/// returns a bool similar to as `f` does.
pub(crate) fn many_sep<'a, F>(p: &mut Parser<'a>, sep: SK, wrap: SK, mut f: F) -> bool
where
  F: FnMut(&mut Parser<'a>) -> bool,
{
  let mut ret = false;
  loop {
    let en = p.enter();
    if !f(p) {
      p.abandon(en);
      break;
    }
    ret = true;
    if p.at(sep) {
      p.bump();
      p.exit(en, wrap);
    } else {
      p.exit(en, wrap);
      break;
    }
  }
  ret
}

pub(crate) enum ShouldBreak {
  Yes,
  No,
  Error,
}

pub(crate) fn should_break(op_info: Infix, min_prec: Infix) -> ShouldBreak {
  if op_info.prec == min_prec.prec && op_info.assoc != min_prec.assoc {
    ShouldBreak::Error
  } else {
    let res = match min_prec.assoc {
      Assoc::Left => op_info.prec <= min_prec.prec,
      Assoc::Right => op_info.prec < min_prec.prec,
    };
    if res {
      ShouldBreak::Yes
    } else {
      ShouldBreak::No
    }
  }
}

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

/// requires we just got a true `name_star_eq(p)`. parses a path. errors if the path is infix.
pub(crate) fn path_no_infix(p: &mut Parser<'_>) {
  let cur = p.peek().unwrap();
  let bad =
    !p.at_n(1, SK::Dot) && matches!(cur.kind, SK::Name | SK::Eq | SK::Star) && p.is_infix(cur.text);
  if bad {
    p.error(ErrorKind::InfixWithoutOp);
  }
  must(p, path, Expected::Path);
}

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
pub(crate) fn name_star_eq(p: &mut Parser<'_>) -> bool {
  name_star(p, 0) || p.at(SK::Eq)
}

/// kind of badly named. it means `Name` or `*`. the `n` is how far to look ahead.
pub(crate) fn name_star(p: &mut Parser<'_>, n: usize) -> bool {
  p.at_n(n, SK::Name) || p.at_n(n, SK::Star)
}

/// see [`name_star`].
pub(crate) fn eat_name_star<'a>(p: &mut Parser<'a>) -> Option<Token<'a, SK>> {
  if name_star(p, 0) {
    Some(p.bump())
  } else {
    p.error(ErrorKind::Expected(Expected::Kind(SK::Name)));
    None
  }
}
