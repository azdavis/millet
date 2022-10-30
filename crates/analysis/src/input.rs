//! Input to analysis.

mod lower_cm;
mod lower_mlb;
mod root_group;
mod topo;
mod util;

use paths::{PathId, PathMap, WithPath};
use util::{ErrorKind, ErrorSource, GroupPathKind, Result};

pub(crate) use root_group::Severities;
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
    let root_group = root_group::RootGroup::new(fs, store, root)?;
    let (sources, groups) = match root_group.kind {
      GroupPathKind::Cm => lower_cm::get(fs, store, &root_group)?,
      GroupPathKind::Mlb => lower_mlb::get(fs, store, &root_group)?,
    };
    let bas_decs = groups.iter().map(|(&a, b)| (a, &b.bas_dec));
    if let Err(err) = topo::check(bas_decs) {
      return Err(Error {
        source: ErrorSource::default(),
        path: store.get_path(err.witness()).as_path().to_owned(),
        kind: ErrorKind::Cycle,
      });
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

  /// Override a source file to have the given contents.
  pub fn override_source(&mut self, path: PathId, contents: String) {
    self.sources.insert(path, contents);
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
