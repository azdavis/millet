//! Getting the root groups.

use crate::types::Severities;
use crate::util::{
  get_path_id, read_dir, str_path, Error, ErrorKind, ErrorSource, GroupPathKind, NoRootFlavor,
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
    let config_path = root.as_path().join(config::file::PATH);
    let config_file = fs.read_to_string(&config_path);
    let ((config, glob), mut flavor) = match config_file {
      Ok(s) => (Config::from_file(root, &config_path, &s, errors), NoRootFlavor::NoGlob),
      Err(_) => ((Config::default(), None), NoRootFlavor::NoFile),
    };
    if let Some(glob) = glob {
      let path = root.as_path().join(glob.as_str());
      glob_root_group_paths(fs, &mut root_group_paths, root, &path, &config_path, errors);
      if root_group_paths.is_empty() {
        errors.push(Error::new(
          ErrorSource::default(),
          config_path.clone(),
          ErrorKind::EmptyGlob(glob.clone()),
        ));
        flavor = NoRootFlavor::EmptyGlob(glob);
      } else {
        root_group_source.path = Some(config_path);
      }
    }
    if root_group_paths.is_empty() {
      let dir_entries = match read_dir(fs, ErrorSource::default(), root.as_path()) {
        Ok(x) => x,
        Err(e) => {
          errors.push(e);
          vec![]
        }
      };
      for entry in dir_entries {
        let group_path = match GroupPathBuf::new(fs, entry.clone()) {
          Some(x) => x,
          None => continue,
        };
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
    if root_group_paths.is_empty() {
      errors.push(Error::new(
        ErrorSource::default(),
        root.as_path().to_owned(),
        ErrorKind::NoRoot(flavor),
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
  pub(crate) lang: config::lang::Language,
}

impl Config {
  fn from_file(
    root: &paths::CanonicalPathBuf,
    config_path: &Path,
    contents: &str,
    errors: &mut Vec<Error>,
  ) -> (Config, Option<str_util::SmolStr>) {
    let mut ret = Config::default();
    let parsed: config::file::Root = match toml::from_str(contents) {
      Ok(x) => x,
      Err(e) => {
        errors.push(Error::new(
          ErrorSource::default(),
          config_path.to_owned(),
          ErrorKind::CouldNotParseConfig(e),
        ));
        return (ret, None);
      }
    };
    if parsed.version != 1 {
      errors.push(Error::new(
        ErrorSource::default(),
        config_path.to_owned(),
        ErrorKind::InvalidConfigVersion(parsed.version),
      ));
    }
    for (key, val) in parsed.workspace.path_vars {
      // we resolve config-root-relative paths here, but we have to wait until later to resolve
      // workspace-root-relative paths.
      let (kind, suffix) = match val {
        config::file::PathVar::Value(val) => (EnvEntryKind::Value, val),
        config::file::PathVar::Path(val) => {
          let path = root.as_path().join(val.as_str());
          let source = ErrorSource { path: Some(config_path.to_owned()), range: None };
          let val = match str_path(source, &path) {
            Ok(x) => str_util::SmolStr::from(x),
            Err(e) => {
              errors.push(e);
              continue;
            }
          };
          (EnvEntryKind::Value, val)
        }
        config::file::PathVar::WorkspacePath(val) => (EnvEntryKind::WorkspacePath, val),
      };
      ret.path_vars.insert(key, EnvEntry { kind, suffix });
    }

    for (code, config) in parsed.diagnostics {
      let code = match code.parse::<diagnostic_util::Code>() {
        Ok(x) => x,
        Err(e) => {
          errors.push(Error::new(
            ErrorSource::default(),
            config_path.to_owned(),
            ErrorKind::InvalidErrorCode(code, e),
          ));
          continue;
        }
      };
      let sev = match config.severity {
        config::file::Severity::Ignore => None,
        config::file::Severity::Warning => Some(diagnostic_util::Severity::Warning),
        config::file::Severity::Error => Some(diagnostic_util::Severity::Error),
      };
      ret.severities.insert(code, sev);
    }
    ret.lang.dec = parsed.language.dec;
    ret.lang.exp = parsed.language.exp;
    for (path, allowed) in parsed.language.val {
      let parts: Option<Vec<_>> = path.split('.').map(str_util::Name::try_new).collect();
      let parts = match parts {
        Some(x) => x,
        None => {
          errors.push(Error::new(
            ErrorSource::default(),
            config_path.to_owned(),
            ErrorKind::EmptyStrInPath(path),
          ));
          continue;
        }
      };
      if !allowed {
        let p = sml_path::Path::try_new(parts).expect("split always returns non-empty iter");
        ret.lang.val.insert(p);
      }
    }
    (ret, parsed.workspace.root)
  }
}

fn glob_root_group_paths<F>(
  fs: &F,
  root_group_paths: &mut Vec<GroupPathBuf>,
  root: &paths::CanonicalPathBuf,
  path: &Path,
  config_path: &Path,
  errors: &mut Vec<Error>,
) where
  F: paths::FileSystem,
{
  let glob = str_path(ErrorSource { path: Some(config_path.to_owned()), range: None }, path);
  let glob = match glob {
    Ok(x) => x,
    Err(e) => {
      errors.push(e);
      return;
    }
  };
  let paths = match fs.glob(glob) {
    Ok(x) => x,
    Err(e) => {
      errors.push(Error::new(
        ErrorSource::default(),
        config_path.to_owned(),
        ErrorKind::GlobPattern(e),
      ));
      return;
    }
  };
  for path in paths {
    let path = match path {
      Ok(x) => x,
      Err(e) => {
        errors.push(Error::from_io(config_path.to_owned(), e.into_error()));
        continue;
      }
    };
    let path = root.as_path().join(path);
    match GroupPathBuf::new(fs, path.clone()) {
      Some(path) => root_group_paths.push(path),
      None => errors.push(Error::new(
        ErrorSource { path: Some(config_path.to_owned()), range: None },
        path,
        ErrorKind::NotGroup,
      )),
    }
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
