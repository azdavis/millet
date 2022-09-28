use crate::input::util::GroupPathKind;
use std::path::{Path, PathBuf};

/// A group path.
#[derive(Debug)]
pub(crate) struct GroupPath {
  kind: GroupPathKind,
  path: PathBuf,
}

impl GroupPath {
  /// Returns a new `GroupPath`.
  pub(crate) fn new<F>(fs: &F, path: PathBuf) -> Option<GroupPath>
  where
    F: paths::FileSystem,
  {
    if !fs.is_file(path.as_path()) {
      return None;
    }
    let kind = match path.extension()?.to_str()? {
      "cm" => GroupPathKind::Cm,
      "mlb" => GroupPathKind::Mlb,
      _ => return None,
    };
    Some(GroupPath { path, kind })
  }

  /// Return the kind of path this is.
  pub(crate) fn kind(&self) -> GroupPathKind {
    self.kind
  }

  /// Return this as a `Path`.
  pub(crate) fn as_path(&self) -> &Path {
    self.path.as_path()
  }
}
