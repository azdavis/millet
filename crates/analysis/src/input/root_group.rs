//! Get the root group.

use crate::input::util::{
  get_path_id, read_dir, ErrorSource, GetInputErrorKind, GroupPathKind, InputError, Result,
};
use paths::PathId;
use std::path::PathBuf;

pub(crate) struct RootGroup {
  pub(crate) path: PathId,
  pub(crate) kind: GroupPathKind,
  pub(crate) path_vars: paths::slash_var_path::Env,
}

impl RootGroup {
  pub(crate) fn new<F>(fs: &F, root: &mut paths::Root) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let mut root_group_source = ErrorSource::default();
    let mut root_group_path = None::<GroupPathBuf>;
    let config_path = root.as_path().join(config::FILE_NAME);
    let path_vars = match fs.read_to_string(&config_path) {
      Ok(contents) => {
        let config = Config::new(fs, root, config_path, contents.as_str())?;
        if let Some(path) = config.root_group {
          root_group_path = Some(path);
          root_group_source.path = Some(config.path);
        }
        config.path_vars
      }
      Err(_) => paths::slash_var_path::Env::default(),
    };
    if root_group_path.is_none() {
      let dir_entries = read_dir(fs, ErrorSource::default(), root.as_path())?;
      for entry in dir_entries {
        if let Some(group_path) = GroupPathBuf::new(fs, entry.clone()) {
          match &root_group_path {
            Some(rgp) => {
              return Err(InputError {
                kind: GetInputErrorKind::MultipleRoots(rgp.path.to_owned(), entry.clone()),
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
          kind: GetInputErrorKind::NoRoot,
        })
      }
    };
    Ok(Self {
      path: get_path_id(fs, root, root_group_source, &root_group_path.path)?,
      kind: root_group_path.kind,
      path_vars,
    })
  }
}

struct Config {
  path: PathBuf,
  path_vars: paths::slash_var_path::Env,
  root_group: Option<GroupPathBuf>,
}

impl Config {
  fn new<F>(fs: &F, root: &paths::Root, config_path: PathBuf, contents: &str) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let mut ret = Self {
      path: config_path,
      path_vars: paths::slash_var_path::Env::default(),
      root_group: None,
    };
    let parsed: config::Root = match toml::from_str(contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(InputError {
          source: ErrorSource::default(),
          path: ret.path,
          kind: GetInputErrorKind::CouldNotParseConfig(e),
        })
      }
    };
    if parsed.version != 1 {
      return Err(InputError {
        source: ErrorSource::default(),
        path: ret.path,
        kind: GetInputErrorKind::InvalidConfigVersion(parsed.version),
      });
    }
    let ws = match parsed.workspace {
      Some(x) => x,
      None => return Ok(ret),
    };
    if let Some(members) = ws.members {
      if ws.root.is_some() || ws.path_vars.is_some() {
        return Err(InputError {
          source: ErrorSource::default(),
          path: ret.path,
          kind: GetInputErrorKind::HasMembersButAlsoRootOrPathVars,
        });
      }
      // TODO
      log::error!("unsupported use of members: {members:?}");
    }
    if let Some(ws_path_vars) = ws.path_vars {
      for (key, val) in ws_path_vars {
        match val {
          config::PathVar::Value(val) => {
            ret.path_vars.insert(key, val);
          }
          config::PathVar::Path(p) => {
            let val: str_util::SmolStr = root.as_path().join(p.as_str()).to_string_lossy().into();
            ret.path_vars.insert(key, val);
          }
        }
      }
    }
    if let Some(path) = ws.root {
      let path = root.as_path().join(path.as_str());
      match GroupPathBuf::new(fs, path.clone()) {
        Some(path) => ret.root_group = Some(path),
        None => {
          return Err(InputError {
            source: ErrorSource { path: Some(ret.path), range: None },
            path,
            kind: GetInputErrorKind::NotGroup,
          })
        }
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
