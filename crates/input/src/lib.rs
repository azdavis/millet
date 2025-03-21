//! Input to analysis.

mod lower_cm;
mod lower_mlb;
mod root;
mod topo;
mod types;
mod util;

use paths::{PathId, PathMap};
use util::{ErrorKind, ErrorSource, GroupPathKind};

pub use types::{Group, Severities};
pub use util::Error;

/// The input to analysis.
#[derive(Debug, Default)]
pub struct Input {
  /// A map from source paths to their contents.
  pub sources: PathMap<String>,
  /// A map from group paths to their (parsed) contents.
  pub groups: PathMap<types::Group>,
  /// The root group ids.
  pub root_group_paths: Vec<PathId>,
  /// Severities to override.
  pub severities: types::Severities,
  /// The language config.
  pub lang: config::lang::Language,
  /// Errors when getting input.
  pub errors: Vec<Error>,
}

impl Input {
  /// Get input anchored at the root.
  ///
  /// # Errors
  ///
  /// When getting input failed.
  ///
  /// # Panics
  ///
  /// When the path has no parent, or other such weird cases.
  pub fn new<F>(fs: &F, paths: &mut paths::Store, root: &paths::CleanPath) -> Input
  where
    F: paths_glob::FileSystem,
  {
    let mut ret = Input::default();
    let root = root::Root::new(fs, paths, root, &mut ret.errors);
    ret.severities = root.config.severities;
    ret.lang = root.config.lang;
    for group in root.groups {
      let path = paths.get_path(group.path).as_path();
      let parent = path.parent().expect("should have parent for group path");
      let parent = match util::str_path(ErrorSource::default(), parent) {
        Ok(x) => x,
        Err(e) => {
          ret.errors.push(e);
          continue;
        }
      };
      let path_var_env = slash_var_path::resolve_env(parent, root.config.path_vars.clone());
      let f = match group.kind {
        GroupPathKind::Cm => lower_cm::get,
        GroupPathKind::Mlb => lower_mlb::get,
      };
      f(fs, &mut ret.sources, &mut ret.groups, paths, &path_var_env, group.path, &mut ret.errors);
      ret.root_group_paths.push(group.path);
    }
    let bas_decs = ret.groups.iter().map(|(&a, b)| (a, &b.bas_dec));
    if let Err(err) = topo::check(bas_decs) {
      ret.errors.push(Error::new(
        ErrorSource::default(),
        paths.get_path(err.witness()).as_path().to_owned(),
        ErrorKind::Cycle,
      ));
      // TODO only clear out the problematic files
      ret.sources.clear();
      ret.groups.clear();
      ret.root_group_paths.clear();
    }
    ret
  }
}
