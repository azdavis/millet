//! Types for working with paths.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use fast_hash::FxHashMap;
use std::path::{Path, PathBuf, StripPrefixError};

/// A root, in which all files are contained.
#[derive(Debug)]
pub struct Root {
  root: CanonicalPathBuf,
  id_to_path: Vec<CanonicalPathBuf>,
  path_to_id: FxHashMap<CanonicalPathBuf, PathId>,
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
    // don't store the suffix, but compute it.
    let _ = path.as_path().strip_prefix(self.root.as_path())?;
    let id = match self.path_to_id.get(path) {
      Some(x) => *x,
      None => {
        let id = PathId(self.id_to_path.len());
        self.id_to_path.push(path.clone());
        assert!(self.path_to_id.insert(path.clone(), id).is_none());
        id
      }
    };
    Ok(id)
  }

  /// Returns the path for this ID.
  pub fn get_path(&self, id: PathId) -> &CanonicalPathBuf {
    &self.id_to_path[id.0]
  }

  /// Returns the path relative to the root for this ID.
  pub fn get_rel_path(&self, id: PathId) -> &Path {
    self.id_to_path[id.0]
      .as_path()
      .strip_prefix(self.root.as_path())
      .expect("we should not give an id unless the path is in the root")
  }
}

/// A path identifier. Cheap to copy and compare.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathId(usize);

/// A map from paths to something.
pub type PathMap<T> = FxHashMap<PathId, T>;

/// A canonical (and therefore absolute) path buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CanonicalPathBuf(PathBuf);

impl CanonicalPathBuf {
  /// Returns the underlying [`Path`].
  pub fn as_path(&self) -> &Path {
    self.0.as_path()
  }

  /// Turns this into a [`PathBuf`].
  pub fn into_path_buf(self) -> PathBuf {
    self.0
  }
}

/// A file system.
pub trait FileSystem {
  /// Read the contents of a file.
  fn read_to_string(&self, path: &Path) -> std::io::Result<String>;
  /// Canonicalize a path.
  fn canonicalize(&self, path: &Path) -> std::io::Result<CanonicalPathBuf>;
  /// Read the entries of a directory. The vec is in arbitrary order.
  fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>>;
  /// Returns whether this is a file. If unknown, returns false.
  fn is_file(&self, path: &Path) -> bool;
}

/// The real file system. Does actual I/O.
#[derive(Debug, Default)]
pub struct RealFileSystem(());

impl FileSystem for RealFileSystem {
  fn read_to_string(&self, path: &Path) -> std::io::Result<String> {
    std::fs::read_to_string(path)
  }

  fn canonicalize(&self, path: &Path) -> std::io::Result<CanonicalPathBuf> {
    Ok(CanonicalPathBuf(std::fs::canonicalize(path)?))
  }

  fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
    std::fs::read_dir(path)?.map(|x| Ok(x?.path())).collect()
  }

  fn is_file(&self, path: &Path) -> bool {
    path.is_file()
  }
}

/// A 'file system' in memory.
///
/// Doesn't totally handle all `Path`s. For instance, it probably gives unexpected results for paths
/// that:
/// - Have trailing `/`
/// - Have `.`
/// - Have `..`
/// - Do not start with `/`
///
/// But this is mainly intended for basic testing purposes, so it's fine.
#[derive(Debug, Default)]
pub struct MemoryFileSystem(FxHashMap<PathBuf, String>);

impl MemoryFileSystem {
  /// Returns a new `MemoryFileSystem`.
  pub fn new(map: FxHashMap<PathBuf, String>) -> Self {
    Self(map)
  }
}

impl FileSystem for MemoryFileSystem {
  fn read_to_string(&self, path: &Path) -> std::io::Result<String> {
    match self.0.get(path) {
      Some(x) => Ok(x.clone()),
      None => Err(std::io::Error::from(std::io::ErrorKind::NotFound)),
    }
  }

  fn canonicalize(&self, path: &Path) -> std::io::Result<CanonicalPathBuf> {
    if self.0.contains_key(path) {
      Ok(CanonicalPathBuf(path.to_owned()))
    } else {
      Err(std::io::Error::from(std::io::ErrorKind::NotFound))
    }
  }

  fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
    Ok(
      self
        .0
        .keys()
        .filter_map(|p| (p.starts_with(path) && p != path).then(|| p.to_owned()))
        .collect(),
    )
  }

  fn is_file(&self, path: &Path) -> bool {
    self.0.contains_key(path)
  }
}
