//! Extends the paths file system trait with globs.

pub use wax::{BuildError, WalkError};

/// An extended fs trait for globs.
pub trait FileSystem: paths::FileSystem {
  /// The type of globs.
  type Glob<'a>
  where
    Self: 'a;

  /// Build a glob.
  ///
  /// # Errors
  ///
  /// When constructing the glob failed e.g. because of invalid pattern syntax.
  fn glob(pattern: &str) -> Result<Self::Glob<'_>, BuildError>;

  /// A walked entry.
  type WalkEntry<'a>;

  /// The iterator type of walks.
  type Walk<'a>: Iterator<Item = Result<Self::WalkEntry<'static>, WalkError>>
  where
    Self: 'a;

  /// Walk the current directory.
  fn walk<'g>(&self, glob: &'g Self::Glob<'_>) -> Self::Walk<'g>;

  /// Turns an entry into its path.
  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path;
}

impl FileSystem for paths::RealFileSystem {
  type Glob<'a> = wax::Glob<'a>;

  fn glob(pattern: &str) -> Result<Self::Glob<'_>, BuildError> {
    wax::Glob::new(pattern)
  }

  type WalkEntry<'a> = wax::WalkEntry<'a>;

  type Walk<'a> = wax::Walk<'a>;

  fn walk<'g>(&self, glob: &'g Self::Glob<'_>) -> Self::Walk<'g> {
    glob.walk(".")
  }

  /// Turns an entry into its path.
  fn entry_path<'e>(entry: &'e Self::WalkEntry<'_>) -> &'e std::path::Path {
    entry.path()
  }
}

impl FileSystem for paths::MemoryFileSystem {
  type Glob<'a> = &'a str;

  fn glob(pattern: &str) -> Result<Self::Glob<'_>, BuildError> {
    Ok(pattern)
  }

  type WalkEntry<'a> = paths::CleanPathBuf;

  type Walk<'a> = std::vec::IntoIter<Result<Self::WalkEntry<'static>, WalkError>>;

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
