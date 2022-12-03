//! Input to analysis.

mod lower_cm;
mod lower_mlb;
mod root;
mod topo;
mod util;

use paths::{PathId, PathMap, WithPath};
use util::{ErrorKind, ErrorSource, GroupPathKind, Result};

pub(crate) use root::Severities;
pub use util::Error;

/// The input to analysis.
#[derive(Debug)]
pub struct Input {
  /// A map from source paths to their contents.
  pub(crate) sources: PathMap<String>,
  /// A map from group paths to their (parsed) contents.
  pub(crate) groups: PathMap<Group>,
  /// The root group id.
  pub(crate) root_group_paths: Vec<PathId>,
  /// Severities to override.
  pub(crate) severities: Severities,
}

impl Input {
  /// Get input anchored at the root.
  ///
  /// # Errors
  ///
  /// When getting input failed.
  pub fn new<F>(fs: &F, store: &mut paths::Store, root: &paths::CanonicalPathBuf) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let root = root::Root::new(fs, store, root)?;
    let mut ret = Self {
      sources: PathMap::default(),
      groups: PathMap::default(),
      root_group_paths: Vec::new(),
      severities: root.config.severities,
    };
    for group in root.groups {
      let parent =
        store.get_path(group.path).as_path().parent().expect("group path with no parent");
      let path_var_env = paths::slash_var_path::resolve_env(parent, root.config.path_vars.clone());
      let f = match group.kind {
        GroupPathKind::Cm => lower_cm::get,
        GroupPathKind::Mlb => lower_mlb::get,
      };
      f(fs, &mut ret.sources, &mut ret.groups, store, &path_var_env, group.path)?;
      ret.root_group_paths.push(group.path);
    }
    let bas_decs = ret.groups.iter().map(|(&a, b)| (a, &b.bas_dec));
    if let Err(err) = topo::check(bas_decs) {
      return Err(Error::new(
        ErrorSource::default(),
        store.get_path(err.witness()).as_path().to_owned(),
        ErrorKind::Cycle,
      ));
    }
    Ok(ret)
  }

  /// Return an iterator over the source paths.
  pub fn iter_sources(&self) -> impl Iterator<Item = WithPath<&str>> + '_ {
    self.sources.iter().map(|(&path, s)| path.wrap(s.as_str()))
  }

  /// Returns a mutable ref to the source for this path.
  pub fn get_mut_source(&mut self, path: PathId) -> Option<&mut String> {
    self.sources.get_mut(&path)
  }
}

/// A description of how to check a group of source files.
#[derive(Debug)]
pub(crate) struct Group {
  /// A lowered BasDec, describing the group.
  pub(crate) bas_dec: mlb_statics::BasDec,
  /// A position DB for the group file that yielded the dec.
  pub(crate) pos_db: text_pos::PositionDb,
}
