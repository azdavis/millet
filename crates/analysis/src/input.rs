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
  pub(crate) root_group_id: PathId,
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
    let root_group = root::RootGroup::new(fs, store, root)?;
    let mut sources = PathMap::<String>::default();
    let mut groups = PathMap::<Group>::default();
    match root_group.kind {
      GroupPathKind::Cm => lower_cm::get(
        fs,
        &mut sources,
        &mut groups,
        store,
        &root_group.config.path_vars,
        root_group.path,
      )?,
      GroupPathKind::Mlb => lower_mlb::get(
        fs,
        &mut sources,
        &mut groups,
        store,
        &root_group.config.path_vars,
        root_group.path,
      )?,
    }
    let bas_decs = groups.iter().map(|(&a, b)| (a, &b.bas_dec));
    if let Err(err) = topo::check(bas_decs) {
      return Err(Error::new(
        ErrorSource::default(),
        store.get_path(err.witness()).as_path().to_owned(),
        ErrorKind::Cycle,
      ));
    }
    Ok(Self {
      sources,
      groups,
      root_group_id: root_group.path,
      severities: root_group.config.severities,
    })
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
