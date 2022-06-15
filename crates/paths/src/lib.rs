//! Types for working with paths.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use std::path::{Path, PathBuf, StripPrefixError};

use fast_hash::FxHashMap;

/// A root, in which all files are contained.
#[derive(Debug)]
pub struct Root {
  root: CanonicalPathBuf,
  id_to_path: Vec<PathBuf>,
  path_to_id: FxHashMap<PathBuf, PathId>,
}

impl Root {
  /// Returns a new `Root` rooted at `root`.
  pub fn new(root: CanonicalPathBuf) -> Self {
    Self {
      root,
      id_to_path: Vec::new(),
      path_to_id: FxHashMap::default(),
    }
  }

  /// Returns the path underlying this `Root`.
  pub fn as_path(&self) -> &Path {
    self.root.as_path()
  }

  /// Returns an ID for this path, if the path is in the root.
  pub fn get_id(&mut self, path: &CanonicalPathBuf) -> Result<PathId, StripPrefixError> {
    let path = path.as_path().strip_prefix(self.root.as_path())?;
    let id = match self.path_to_id.get(path) {
      Some(x) => *x,
      None => {
        let path = path.to_owned();
        let id = PathId(self.id_to_path.len());
        self.id_to_path.push(path.clone());
        assert!(self.path_to_id.insert(path, id).is_none());
        id
      }
    };
    Ok(id)
  }

  /// Returns the path for this ID.
  pub fn get_path(&self, id: PathId) -> &Path {
    &self.id_to_path[id.0]
  }
}

/// A path identifier. Cheap to copy and compare.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathId(usize);

// only used for testing
impl PathId {
  #[doc(hidden)]
  pub fn from_raw(n: usize) -> Self {
    Self(n)
  }

  #[doc(hidden)]
  pub fn into_raw(self) -> usize {
    self.0
  }
}

/// A canonical (and therefore absolute) path buffer.
#[derive(Debug, Clone)]
pub struct CanonicalPathBuf(PathBuf);

impl CanonicalPathBuf {
  /// Returns the underlying [`Path`].
  pub fn as_path(&self) -> &Path {
    self.0.as_path()
  }
}

impl<'a> TryFrom<&'a Path> for CanonicalPathBuf {
  type Error = std::io::Error;

  fn try_from(path: &'a Path) -> Result<Self, Self::Error> {
    Ok(Self(path.canonicalize()?))
  }
}
