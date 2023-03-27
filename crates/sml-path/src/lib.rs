//! A path, a non-empty sequence of names.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

use std::fmt;
use str_util::Name;

/// A path, a non-empty sequence of names.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
  prefix: Vec<Name>,
  last: Name,
}

impl Path {
  pub fn new<I>(prefix: I, last: Name) -> Self
  where
    I: IntoIterator<Item = Name>,
  {
    Self { prefix: prefix.into_iter().collect(), last }
  }

  #[must_use]
  pub fn try_new(mut names: Vec<Name>) -> Option<Self> {
    let last = names.pop()?;
    Some(Self::new(names, last))
  }

  #[must_use]
  pub fn one(name: Name) -> Self {
    Self::new(Vec::new(), name)
  }

  #[must_use]
  pub fn last(&self) -> &Name {
    &self.last
  }

  #[must_use]
  pub fn prefix(&self) -> &[Name] {
    &self.prefix
  }

  pub fn all_names(&self) -> impl Iterator<Item = &Name> {
    self.prefix.iter().chain(std::iter::once(&self.last))
  }
}

impl fmt::Display for Path {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for name in &self.prefix {
      name.fmt(f)?;
      f.write_str(".")?;
    }
    self.last.fmt(f)
  }
}
