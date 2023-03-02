//! Getting the root groups.

use crate::types::Severities;
use crate::util::{
  get_path_id, read_dir, str_path, Error, ErrorKind, ErrorSource, GroupPathKind, Result,
};
use paths::PathId;
use slash_var_path::{EnvEntry, EnvEntryKind};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub(crate) struct Root {
  pub(crate) groups: Vec<RootGroup>,
  pub(crate) config: Config,
}

impl Root {
  pub(crate) fn new<F>(
    fs: &F,
    store: &mut paths::Store,
    root: &paths::CanonicalPathBuf,
    errors: &mut Vec<Error>,
  ) -> Root
  where
    F: paths::FileSystem,
  {
    let mut root_group_source = ErrorSource::default();
    let mut root_group_paths = Vec::<GroupPathBuf>::new();
    let config_path = root.as_path().join(config::FILE_NAME);
    let config_file = fs.read_to_string(&config_path);
    let had_config_file = config_file.is_ok();
    let config = match config_file {
      Ok(contents) => {
        match Config::from_file(fs, &mut root_group_paths, root, &config_path, contents.as_str()) {
          Ok(config) => {
            if !root_group_paths.is_empty() {
              root_group_source.path = Some(config_path);
            }
            config
          }
          Err(e) => {
            errors.push(e);
            Config::default()
          }
        }
      }
      Err(_) => Config::default(),
    };
    if root_group_paths.is_empty() {
      let dir_entries = match read_dir(fs, ErrorSource::default(), root.as_path()) {
        Ok(x) => x,
        Err(e) => {
          errors.push(e);
          vec![]
        }
      };
      for entry in dir_entries {
        if let Some(group_path) = GroupPathBuf::new(fs, entry.clone()) {
          match root_group_paths.first() {
            Some(rgp) => errors.push(Error::new(
              ErrorSource { path: Some(rgp.path.clone()), range: None },
              entry.clone(),
              ErrorKind::MultipleRoots(rgp.path.clone(), entry),
            )),
            None => root_group_paths.push(group_path),
          }
        }
      }
    }
    if root_group_paths.is_empty() {
      errors.push(Error::new(
        ErrorSource::default(),
        root.as_path().to_owned(),
        ErrorKind::NoRoot(had_config_file),
      ));
    }
    let mut ret = Root { groups: Vec::new(), config };
    for root_group_path in root_group_paths {
      match get_path_id(fs, store, root_group_source.clone(), &root_group_path.path) {
        Ok(path) => ret.groups.push(RootGroup { path, kind: root_group_path.kind }),
        Err(e) => errors.push(e),
      }
    }
    ret
  }
}

#[derive(Debug)]
pub(crate) struct RootGroup {
  pub(crate) path: PathId,
  pub(crate) kind: GroupPathKind,
}

#[derive(Debug, Default)]
pub(crate) struct Config {
  pub(crate) path_vars: slash_var_path::UnresolvedEnv,
  pub(crate) severities: Severities,
}

impl Config {
  #[allow(clippy::too_many_lines)]
  fn from_file<F>(
    fs: &F,
    root_group_paths: &mut Vec<GroupPathBuf>,
    root: &paths::CanonicalPathBuf,
    config_path: &Path,
    contents: &str,
  ) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let mut ret = Config::default();
    let parsed: config::Root = match toml::from_str(contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(Error::new(
          ErrorSource::default(),
          config_path.to_owned(),
          ErrorKind::CouldNotParseConfig(e),
        ))
      }
    };
    if parsed.version != 1 {
      return Err(Error::new(
        ErrorSource::default(),
        config_path.to_owned(),
        ErrorKind::InvalidConfigVersion(parsed.version),
      ));
    }
    if let Some(ws) = parsed.workspace {
      if let Some(root_path_glob) = ws.root {
        let path = root.as_path().join(root_path_glob.as_str());
        let glob =
          str_path(ErrorSource { path: Some(config_path.to_owned()), range: None }, &path)?;
        let paths = match fs.glob(glob.as_ref()) {
          Ok(x) => x,
          Err(e) => {
            return Err(Error::new(
              ErrorSource::default(),
              config_path.to_owned(),
              ErrorKind::GlobPattern(e),
            ))
          }
        };
        for path in paths {
          let path = match path {
            Ok(x) => x,
            Err(e) => {
              return Err(Error::new(
                ErrorSource::default(),
                config_path.to_owned(),
                ErrorKind::Io(e.into_error()),
              ))
            }
          };
          let path = root.as_path().join(path);
          match GroupPathBuf::new(fs, path.clone()) {
            Some(path) => root_group_paths.push(path),
            None => {
              return Err(Error::new(
                ErrorSource { path: Some(config_path.to_owned()), range: None },
                path,
                ErrorKind::NotGroup,
              ))
            }
          }
        }
        if root_group_paths.is_empty() {
          return Err(Error::new(
            ErrorSource::default(),
            config_path.to_owned(),
            ErrorKind::EmptyGlob(root_path_glob),
          ));
        }
      }
      if let Some(ws_path_vars) = ws.path_vars {
        for (key, val) in ws_path_vars {
          // we resolve config-root-relative paths here, but we have to wait until later to resolve
          // workspace-root-relative paths.
          let (kind, suffix) = match val {
            config::PathVar::Value(val) => (EnvEntryKind::Value, val),
            config::PathVar::Path(val) => {
              let path = root.as_path().join(val.as_str());
              let val: str_util::SmolStr =
                str_path(ErrorSource { path: Some(config_path.to_owned()), range: None }, &path)?
                  .into();
              (EnvEntryKind::Value, val)
            }
            config::PathVar::WorkspacePath(val) => (EnvEntryKind::WorkspacePath, val),
          };
          ret.path_vars.insert(key, EnvEntry { kind, suffix });
        }
      }
    }
    for (code, config) in parsed.diagnostics.into_iter().flatten() {
      let code = match code.parse::<diagnostic_util::Code>() {
        Ok(x) => x,
        Err(e) => {
          return Err(Error::new(
            ErrorSource::default(),
            config_path.to_owned(),
            ErrorKind::InvalidErrorCode(code, e),
          ));
        }
      };
      if let Some(sev) = config.severity {
        let sev = match sev {
          config::Severity::Ignore => None,
          config::Severity::Warning => Some(diagnostic_util::Severity::Warning),
          config::Severity::Error => Some(diagnostic_util::Severity::Error),
        };
        ret.severities.insert(code, sev);
      }
    }
    Ok(ret)
  }
}

#[derive(Debug)]
struct GroupPathBuf {
  kind: GroupPathKind,
  path: PathBuf,
}

impl GroupPathBuf {
  fn new<F>(fs: &F, path: PathBuf) -> Option<GroupPathBuf>
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
    Some(GroupPathBuf { kind, path })
  }
}
