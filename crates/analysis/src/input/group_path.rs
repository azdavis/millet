//! Deal with group paths.

use crate::input::util::{get_path_id, ErrorSource, GetInputError, GetInputErrorKind, Result};
use paths::PathId;
use std::path::{Path, PathBuf};

/// The root, in which every path is contained.
#[derive(Debug)]
pub struct Root {
  paths: paths::Root,
  group_path: Option<GroupPath>,
}

impl Root {
  pub(crate) fn new(paths: paths::Root, group_path: Option<GroupPath>) -> Self {
    Self { paths, group_path }
  }

  /// Returns this as a paths root.
  pub fn as_paths(&self) -> &paths::Root {
    &self.paths
  }

  /// Returns this as a mutable paths root.
  pub fn as_mut_paths(&mut self) -> &mut paths::Root {
    &mut self.paths
  }
}

/// Get a `Root` from a canonical root dir.
pub fn get_root_dir(path: paths::CanonicalPathBuf) -> Root {
  Root { paths: paths::Root::new(path), group_path: None }
}

/// A kind of group path.
#[derive(Debug, Clone, Copy)]
pub(crate) enum GroupPathKind {
  /// SML/NJ Compilation Manager files.
  Cm,
  /// ML Basis files.
  Mlb,
}

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

  /// Return this as a `Path`.
  pub(crate) fn as_path(&self) -> &Path {
    self.path.as_path()
  }
}

pub(crate) struct RootGroupPath {
  pub(crate) path: PathId,
  pub(crate) kind: GroupPathKind,
  pub(crate) path_vars: paths::slash_var_path::Env,
}

pub(crate) fn get_root_group_path<F>(fs: &F, root: &mut Root) -> Result<RootGroupPath>
where
  F: paths::FileSystem,
{
  let mut root_group_source = ErrorSource::default();
  let config_path = root.paths.as_path().join(config::FILE_NAME);
  let mut path_vars = paths::slash_var_path::Env::default();
  if let Ok(contents) = fs.read_to_string(&config_path) {
    let config: config::Root = match toml::from_str(&contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(GetInputError {
          source: ErrorSource::default(),
          path: config_path,
          kind: GetInputErrorKind::CouldNotParseConfig(e),
        })
      }
    };
    if config.version != 1 {
      return Err(GetInputError {
        source: ErrorSource::default(),
        path: config_path,
        kind: GetInputErrorKind::InvalidConfigVersion(config.version),
      });
    }
    if let Some(ws) = config.workspace {
      let has_members = ws.members.is_some();
      let is_leaf = ws.root.is_some() || ws.path_vars.is_some();
      if has_members && is_leaf {
        return Err(GetInputError {
          source: ErrorSource::default(),
          path: config_path,
          kind: GetInputErrorKind::HasMembersButAlsoRootOrPathVars,
        });
      }
      if let Some(ws_path_vars) = ws.path_vars {
        for (key, val) in ws_path_vars {
          match val {
            config::PathVar::Value(val) => {
              path_vars.insert(key, val);
            }
            config::PathVar::Path(p) => {
              let val: str_util::SmolStr =
                root.paths.as_path().join(p.as_str()).to_string_lossy().into();
              path_vars.insert(key, val);
            }
          }
        }
      }
      // try to get from the config.
      if let (None, Some(path)) = (&root.group_path, ws.root) {
        let path = root.paths.as_path().join(path.as_str());
        match GroupPath::new(fs, path.clone()) {
          Some(path) => {
            root_group_source.path = Some(config_path);
            root.group_path = Some(path);
          }
          None => {
            return Err(GetInputError {
              source: ErrorSource { path: Some(config_path), range: None },
              path,
              kind: GetInputErrorKind::NotGroup,
            })
          }
        }
      }
    }
  }
  // if not, try to get one from the root dir.
  if root.group_path.is_none() {
    let dir_entries = fs.read_dir(root.paths.as_path()).map_err(|e| GetInputError {
      source: ErrorSource::default(),
      path: root.paths.as_path().to_owned(),
      kind: GetInputErrorKind::Io(e),
    })?;
    for entry in dir_entries {
      if let Some(group_path) = GroupPath::new(fs, entry.clone()) {
        match &root.group_path {
          Some(rgp) => {
            return Err(GetInputError {
              kind: GetInputErrorKind::MultipleRoots(rgp.path.clone(), entry.clone()),
              source: ErrorSource { path: Some(rgp.path.clone()), range: None },
              path: entry,
            })
          }
          None => root.group_path = Some(group_path),
        }
      }
    }
  }
  let root_group_path = root.group_path.as_ref().ok_or_else(|| GetInputError {
    source: ErrorSource::default(),
    path: root.paths.as_path().to_owned(),
    kind: GetInputErrorKind::NoRoot,
  })?;
  Ok(RootGroupPath {
    path: get_path_id(fs, &mut root.paths, root_group_source, root_group_path.path.as_path())?,
    kind: root_group_path.kind,
    path_vars,
  })
}
