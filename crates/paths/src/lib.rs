//! Types for working with paths.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::single_match_else)]

pub mod slash_var_path;

use fast_hash::FxHashMap;
use std::path::{Path, PathBuf};

/// A store of paths.
#[derive(Debug, Default)]
pub struct Store {
  id_to_path: Vec<CanonicalPathBuf>,
  path_to_id: FxHashMap<CanonicalPathBuf, PathId>,
}

impl Store {
  /// Returns a new `Store`.
  #[must_use]
  pub fn new() -> Self {
    Store::default()
  }

  /// Returns an ID for this path.
  pub fn get_id(&mut self, path: &CanonicalPathBuf) -> PathId {
    match self.path_to_id.get(path) {
      Some(x) => *x,
      None => {
        let id = PathId(idx::Idx::new(self.id_to_path.len()));
        self.id_to_path.push(path.clone());
        self.path_to_id.insert(path.clone(), id);
        id
      }
    }
  }

  /// Returns the path for this ID.
  #[must_use]
  pub fn get_path(&self, id: PathId) -> &CanonicalPathBuf {
    &self.id_to_path[id.0.to_usize()]
  }
}

/// A path identifier. Cheap to copy and compare.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathId(idx::Idx);

impl PathId {
  /// Wrap a value with the path id.
  pub fn wrap<T>(self, val: T) -> WithPath<T> {
    WithPath { path: self, val }
  }
}

/// A pair of path id and value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithPath<T> {
  /// The path id.
  pub path: PathId,
  /// The value.
  pub val: T,
}

/// A map from paths to something.
pub type PathMap<T> = FxHashMap<PathId, T>;

/// A canonical (and therefore absolute) path buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CanonicalPathBuf(PathBuf);

impl CanonicalPathBuf {
  /// Returns the underlying [`Path`].
  #[must_use]
  pub fn as_path(&self) -> &Path {
    self.0.as_path()
  }

  /// Turns this into a [`PathBuf`].
  #[must_use]
  pub fn into_path_buf(self) -> PathBuf {
    self.0
  }
}

/// A file system.
pub trait FileSystem {
  /// Read the contents of a file.
  ///
  /// # Errors
  ///
  /// If the filesystem failed us.
  fn read_to_string(&self, path: &Path) -> std::io::Result<String>;
  /// Canonicalize a path.
  ///
  /// # Errors
  ///
  /// If the filesystem failed us.
  fn canonicalize(&self, path: &Path) -> std::io::Result<CanonicalPathBuf>;
  /// Read the entries of a directory. The vec is in arbitrary order.
  ///
  /// # Errors
  ///
  /// If the filesystem failed us.
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
  #[must_use]
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
      self.0.keys().filter_map(|p| (p.starts_with(path) && p != path).then(|| p.clone())).collect(),
    )
  }

  fn is_file(&self, path: &Path) -> bool {
    self.0.contains_key(path)
  }
}
