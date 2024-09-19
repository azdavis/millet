//! Extends the paths file system trait with globs.

use std::fmt;

/// An extended fs trait for globs.
pub trait FileSystem: paths::FileSystem {
  /// The type of globs.
  type Glob<'a>: std::fmt::Debug
  where
    Self: 'a;

  /// An error when making a glob.
  type BuildError: fmt::Debug + fmt::Display;

  /// Build a glob.
  ///
  /// # Errors
  ///
  /// When constructing the glob failed e.g. because of invalid pattern syntax.
  fn glob(pattern: &str) -> Result<Self::Glob<'_>, Self::BuildError>;

  /// A walked entry.
  type WalkEntry<'a>;

  /// An error when walking.
  type WalkError: fmt::Debug + fmt::Display + Into<std::io::Error>;

  /// The iterator type of walks.
  type Walk<'a>: Iterator<Item = Result<Self::WalkEntry<'static>, Self::WalkError>>
  where
    Self: 'a;

  /// Walk the current directory.
  fn walk<'g>(&self, glob: &'g Self::Glob<'_>) -> Self::Walk<'g>;

  /// Turns an entry into its path.
  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path;
}

impl FileSystem for paths::RealFileSystem {
  type Glob<'a> = wax::Glob<'a>;

  type BuildError = wax::BuildError;

  fn glob(pattern: &str) -> Result<Self::Glob<'_>, Self::BuildError> {
    wax::Glob::new(pattern)
  }

  type WalkEntry<'a> = wax::WalkEntry<'a>;

  type Walk<'a> = wax::Walk<'a>;

  type WalkError = wax::WalkError;

  fn walk<'g>(&self, glob: &'g Self::Glob<'_>) -> Self::Walk<'g> {
    glob.walk(".")
  }

  /// Turns an entry into its path.
  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path {
    entry.path()
  }
}

/// A type with no values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Never(std::convert::Infallible);

impl From<Never> for std::io::Error {
  fn from(value: Never) -> Self {
    match value.0 {}
  }
}

impl fmt::Display for Never {
  fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.0 {}
  }
}

impl FileSystem for paths::MemoryFileSystem {
  type Glob<'a> = &'a str;

  type BuildError = Never;

  fn glob(pattern: &str) -> Result<Self::Glob<'_>, Self::BuildError> {
    Ok(pattern)
  }

  type WalkEntry<'a> = paths::CleanPathBuf;

  type WalkError = Never;

  type Walk<'a> = std::vec::IntoIter<Result<Self::WalkEntry<'static>, Self::WalkError>>;

  fn walk<'g>(&self, glob: &'g Self::Glob<'_>) -> Self::Walk<'g> {
    let cs: Vec<_> = std::path::Path::new(glob).components().collect();
    #[allow(clippy::needless_collect)]
    let ret: Vec<_> = self
      .inner
      .keys()
      .filter_map(|path| {
        if cs.len() != path.as_path().components().count() {
          return None;
        }
        cs.iter()
          .zip(path.as_path().components())
          .all(|(&c, p)| c == std::path::Component::Normal(std::ffi::OsStr::new("*")) || c == p)
          .then(|| Ok(path.clone()))
      })
      .collect();
    ret.into_iter()
  }

  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path {
    entry.as_path()
  }
}
