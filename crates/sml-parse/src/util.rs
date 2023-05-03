//! Parse utilities.

use crate::parser::{ErrorKind, Exited, Expected, Parser};
use sml_syntax::{token::Token, SyntaxKind as SK};

/// whether to emit errors for infix violations
#[derive(Debug, Clone, Copy)]
pub(crate) enum InfixErr {
  No,
  Yes,
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

pub(crate) fn comma_sep<'a, F>(p: &mut Parser<'a>, wrap: SK, end: SK, f: F)
where
  F: FnMut(&mut Parser<'a>),
{
  end_sep(p, wrap, SK::Comma, end, f);
}

/// similar to `many_sep`, but:
///
/// - allows 0 occurrences of `f`
/// - `f` is assumed to always consume tokens (doesn't need to return a bool indicating anything)
/// - returns only after eating `end`
pub(crate) fn end_sep<'a, F>(p: &mut Parser<'a>, wrap: SK, sep: SK, end: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a>),
{
  loop {
    if p.at(end) {
      p.bump();
      break;
    }
    let en = p.enter();
    f(p);
    let mut saw = false;
    if p.at(sep) {
      p.bump();
      saw = true;
    }
    p.exit(en, wrap);
    if !saw {
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

pub(crate) fn should_break(
  p: &mut Parser<'_>,
  op_info: sml_fixity::Infix,
  min_prec: sml_fixity::Infix,
) -> bool {
  if op_info.prec == min_prec.prec && op_info.assoc != min_prec.assoc {
    p.error(ErrorKind::SameFixityDiffAssoc);
    false
  } else {
    match min_prec.assoc {
      sml_fixity::Assoc::Left => op_info.prec <= min_prec.prec,
      sml_fixity::Assoc::Right => op_info.prec < min_prec.prec,
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

pub(crate) fn path_must(p: &mut Parser<'_>) -> bool {
  let ret = path(p).is_some();
  if !ret {
    p.error(ErrorKind::Expected(Expected::Path));
  }
  ret
}

/// requires we just saw (but did not bump) an `op` kw. errors if this parses a path that is not
/// infix.
pub(crate) fn path_infix(p: &mut Parser<'_>, fe: &sml_fixity::Env) {
  let bad = !p.at_n(2, SK::Dot)
    && p.peek_n(1).map_or(false, |tok| {
      matches!(tok.kind, SK::Name | SK::Eq | SK::Star) && !fe.contains_key(tok.text)
    });
  if bad {
    p.error(ErrorKind::UnnecessaryOp);
  }
  p.eat(SK::OpKw);
  path_must(p);
}

/// requires we just got a true `name_star_eq(p)`. parses a path. errors if the path is infix.
pub(crate) fn path_no_infix(p: &mut Parser<'_>, fe: &sml_fixity::Env) {
  let cur = p.peek().unwrap();
  let bad = !p.at_n(1, SK::Dot)
    && matches!(cur.kind, SK::Name | SK::Eq | SK::Star)
    && fe.contains_key(cur.text);
  if bad {
    p.error(ErrorKind::InfixWithoutOp);
  }
  path_must(p);
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

/// `:` or `:>`. in many contexts only `:` is acceptable but we handle that in lowering.
pub(crate) fn ascription(p: &mut Parser<'_>) -> bool {
  p.at(SK::Colon) || p.at(SK::ColonGt)
}
