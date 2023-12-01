//! A path, a non-empty sequence of names.

use std::fmt;
use str_util::Name;

/// A path, a non-empty sequence of names.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
  prefix: Vec<Name>,
  last: Name,
}

impl Path {
  /// Returns a new Path.
  pub fn new<I>(prefix: I, last: Name) -> Self
  where
    I: IntoIterator<Item = Name>,
  {
    Self { prefix: prefix.into_iter().collect(), last }
  }

  /// Returns new Path with the last element in the vec as the last path component. Returns None if the
  /// vec is empty.
  #[must_use]
  pub fn try_new(mut names: Vec<Name>) -> Option<Self> {
    let last = names.pop()?;
    Some(Self::new(names, last))
  }

  /// Returns a Path with no prefix.
  #[must_use]
  pub fn one(name: Name) -> Self {
    Self::new(Vec::new(), name)
  }

  /// Returns the last name in the Path.
  ///
  /// For `Foo.Bar.quz` this would return `quz`.
  #[must_use]
  pub fn last(&self) -> &Name {
    &self.last
  }

  /// Returns the prefix of the Path.
  ///
  /// For `Foo.Bar.quz` this would return `[Foo, Bar]`.
  #[must_use]
  pub fn prefix(&self) -> &[Name] {
    &self.prefix
  }

  /// Return an iterator over all the Names in order.
  ///
  /// For `Foo.Bar.quz` this would iterate in the order `Foo, Bar, quz`.
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
