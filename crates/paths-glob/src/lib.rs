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
  fn walk<'g>(&self, glob: &'g mut Self::Glob<'_>) -> Self::Walk<'g>;

  /// Turns an entry into its path.
  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path;
}

struct WaxFileSystem(pub paths::RealFileSystem);

impl paths::FileSystem for WaxFileSystem {
  fn current_dir(&self) -> std::io::Result<paths::CleanPathBuf> {
    self.0.current_dir()
  }

  fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
    self.0.read_to_string(path)
  }

  fn read_to_bytes(&self, path: &std::path::Path) -> std::io::Result<Vec<u8>> {
    self.0.read_to_bytes(path)
  }

  fn read_dir(&self, path: &std::path::Path) -> std::io::Result<Vec<std::path::PathBuf>> {
    self.0.read_dir(path)
  }

  fn is_file(&self, path: &std::path::Path) -> bool {
    self.0.is_file(path)
  }
}

impl FileSystem for WaxFileSystem {
  type Glob<'a> = wax::Glob<'a>;

  type BuildError = wax::BuildError;

  fn glob(pattern: &str) -> Result<Self::Glob<'_>, Self::BuildError> {
    wax::Glob::new(pattern)
  }

  type WalkEntry<'a> = wax::WalkEntry<'a>;

  type Walk<'a> = wax::Walk<'a>;

  type WalkError = wax::WalkError;

  fn walk<'g>(&self, glob: &'g mut Self::Glob<'_>) -> Self::Walk<'g> {
    glob.walk(".")
  }

  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path {
    entry.path()
  }
}

/// A walker.
#[derive(Debug)]
pub struct Walk<'a>(&'a mut glob::Paths);

impl<'a> Iterator for Walk<'a> {
  type Item = Result<std::path::PathBuf, WalkError>;

  fn next(&mut self) -> Option<Self::Item> {
    Some(self.0.next()?.map_err(WalkError))
  }
}

/// An error when walking.
#[derive(Debug)]
pub struct WalkError(glob::GlobError);

impl fmt::Display for WalkError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl From<WalkError> for std::io::Error {
  fn from(value: WalkError) -> Self {
    std::io::Error::other(value.0)
  }
}

impl FileSystem for paths::RealFileSystem {
  type Glob<'a> = glob::Paths;

  type BuildError = glob::PatternError;

  fn glob(pattern: &str) -> Result<Self::Glob<'_>, Self::BuildError> {
    glob::glob(pattern)
  }

  type WalkEntry<'a> = std::path::PathBuf;

  type Walk<'a> = Walk<'a>;

  type WalkError = WalkError;

  fn walk<'g>(&self, glob: &'g mut Self::Glob<'_>) -> Self::Walk<'g> {
    Walk(glob)
  }

  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path {
    entry.as_path()
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

  fn walk<'g>(&self, glob: &'g mut Self::Glob<'_>) -> Self::Walk<'g> {
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
