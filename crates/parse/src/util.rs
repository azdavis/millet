use rustc_hash::FxHashMap;
use syntax::event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

#[derive(Debug)]
pub(crate) struct OpCx<'a>(FxHashMap<&'a str, OpInfo>);

impl<'a> Default for OpCx<'a> {
  fn default() -> Self {
    let mut m = FxHashMap::default();
    m.insert("::", OpInfo::right(5));
    m.insert("=", OpInfo::left(4));
    m.insert(":=", OpInfo::left(3));
    m.insert("div", OpInfo::left(7));
    m.insert("mod", OpInfo::left(7));
    m.insert("*", OpInfo::left(7));
    m.insert("/", OpInfo::left(7));
    m.insert("+", OpInfo::left(6));
    m.insert("-", OpInfo::left(6));
    m.insert("<", OpInfo::left(4));
    m.insert(">", OpInfo::left(4));
    m.insert("<=", OpInfo::left(4));
    m.insert(">=", OpInfo::left(4));
    Self(m)
  }
}

impl<'a> OpCx<'a> {
  pub(crate) fn insert(&mut self, key: &'a str, val: OpInfo) {
    self.0.insert(key, val);
  }

  pub(crate) fn contains_key(&self, key: &'a str) -> bool {
    self.0.contains_key(key)
  }

  pub(crate) fn remove(&mut self, key: &'a str) {
    self.0.remove(key);
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct OpInfo {
  num: usize,
  assoc: Assoc,
}

impl OpInfo {
  /// Returns a new OpInfo with left associativity.
  pub(crate) fn left(num: usize) -> Self {
    Self {
      num,
      assoc: Assoc::Left,
    }
  }

  /// Returns a new OpInfo with right associativity.
  pub(crate) fn right(num: usize) -> Self {
    Self {
      num,
      assoc: Assoc::Right,
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Assoc {
  Left,
  Right,
}

pub(crate) fn must<'a, F>(p: &mut Parser<'a, SK>, f: F)
where
  F: FnOnce(&mut Parser<'a, SK>) -> Option<Exited>,
{
  if f(p).is_none() {
    p.error();
  }
}

/// similar to `many_sep`, but:
/// - always uses `;` as the separator
/// - allows the separator to not be present
/// - in return, `f` must say whether it parsed anything
pub(crate) fn maybe_semi_sep<'a, F>(p: &mut Parser<'a, SK>, wrap: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a, SK>) -> Option<Exited>,
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
pub(crate) fn many_sep<'a, F>(p: &mut Parser<'a, SK>, sep: SK, wrap: SK, mut f: F)
where
  F: FnMut(&mut Parser<'a, SK>),
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

#[must_use]
pub(crate) fn path(p: &mut Parser<'_, SK>) -> Option<Exited> {
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
pub(crate) fn scon(p: &mut Parser<'_, SK>) -> bool {
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
