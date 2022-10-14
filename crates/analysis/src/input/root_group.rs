//! Get the root group.

use crate::input::util::{
  get_path_id, read_dir, ErrorKind, ErrorSource, GroupPathKind, InputError, Result,
};
use fast_hash::FxHashMap;
use paths::PathId;
use std::path::PathBuf;

pub(crate) struct RootGroup {
  pub(crate) path: PathId,
  pub(crate) kind: GroupPathKind,
  pub(crate) config: Config,
}

impl RootGroup {
  pub(crate) fn new<F>(
    fs: &F,
    store: &mut paths::Store,
    root: &paths::CanonicalPathBuf,
  ) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let mut root_group_source = ErrorSource::default();
    let mut root_group_path = None::<GroupPathBuf>;
    let config_path = root.as_path().join(config::FILE_NAME);
    let config = match fs.read_to_string(&config_path) {
      Ok(contents) => {
        let cff = ConfigFromFile::new(fs, root, config_path, contents.as_str())?;
        if let Some(path) = cff.root_group {
          root_group_path = Some(path);
          root_group_source.path = Some(cff.path);
        }
        cff.config
      }
      Err(_) => Config::default(),
    };
    if root_group_path.is_none() {
      let dir_entries = read_dir(fs, ErrorSource::default(), root.as_path())?;
      for entry in dir_entries {
        if let Some(group_path) = GroupPathBuf::new(fs, entry.clone()) {
          match &root_group_path {
            Some(rgp) => {
              return Err(InputError {
                kind: ErrorKind::MultipleRoots(rgp.path.to_owned(), entry.clone()),
                source: ErrorSource { path: Some(rgp.path.to_owned()), range: None },
                path: entry,
              })
            }
            None => root_group_path = Some(group_path),
          }
        }
      }
    }
    let root_group_path = match &root_group_path {
      Some(x) => x,
      None => {
        return Err(InputError {
          source: ErrorSource::default(),
          path: root.as_path().to_owned(),
          kind: ErrorKind::NoRoot,
        })
      }
    };
    Ok(Self {
      path: get_path_id(fs, store, root_group_source, &root_group_path.path)?,
      kind: root_group_path.kind,
      config,
    })
  }
}

#[derive(Default)]
pub(crate) struct Config {
  pub(crate) path_vars: paths::slash_var_path::Env,
  pub(crate) severities: FxHashMap<diagnostic_util::Code, diagnostic_util::Severity>,
}

struct ConfigFromFile {
  path: PathBuf,
  root_group: Option<GroupPathBuf>,
  config: Config,
}

impl ConfigFromFile {
  fn new<F>(
    fs: &F,
    root: &paths::CanonicalPathBuf,
    config_path: PathBuf,
    contents: &str,
  ) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let mut ret = Self { path: config_path, root_group: None, config: Config::default() };
    let parsed: config::Root = match toml::from_str(contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(InputError {
          source: ErrorSource::default(),
          path: ret.path,
          kind: ErrorKind::CouldNotParseConfig(e),
        })
      }
    };
    if parsed.version != 1 {
      return Err(InputError {
        source: ErrorSource::default(),
        path: ret.path,
        kind: ErrorKind::InvalidConfigVersion(parsed.version),
      });
    }
    if let Some(ws) = parsed.workspace {
      if let Some(members) = ws.members {
        if ws.root.is_some() || ws.path_vars.is_some() || parsed.errors.is_some() {
          return Err(InputError {
            source: ErrorSource::default(),
            path: ret.path,
            kind: ErrorKind::HasMembersButAlsoOtherSettings,
          });
        }
        // TODO
        log::error!("unsupported use of members: {members:?}");
      }
      if let Some(path) = ws.root {
        let path = root.as_path().join(path.as_str());
        match GroupPathBuf::new(fs, path.clone()) {
          Some(path) => ret.root_group = Some(path),
          None => {
            return Err(InputError {
              source: ErrorSource { path: Some(ret.path), range: None },
              path,
              kind: ErrorKind::NotGroup,
            })
          }
        }
      }
      if let Some(ws_path_vars) = ws.path_vars {
        for (key, val) in ws_path_vars {
          match val {
            config::PathVar::Value(val) => {
              ret.config.path_vars.insert(key, val);
            }
            config::PathVar::Path(p) => {
              let val: str_util::SmolStr = root.as_path().join(p.as_str()).to_string_lossy().into();
              ret.config.path_vars.insert(key, val);
            }
          }
        }
      }
    }
    for (code, config) in parsed.errors.into_iter().flatten() {
      let code = match code.parse::<diagnostic_util::Code>() {
        Ok(x) => x,
        Err(e) => {
          return Err(InputError {
            source: ErrorSource::default(),
            path: ret.path,
            kind: ErrorKind::InvalidErrorCode(code, e),
          });
        }
      };
      if let Some(sev) = config.severity {
        let sev = match sev {
          config::Severity::Warning => diagnostic_util::Severity::Warning,
          config::Severity::Error => diagnostic_util::Severity::Error,
        };
        ret.config.severities.insert(code, sev);
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
    Some(GroupPathBuf { path, kind })
  }
}
