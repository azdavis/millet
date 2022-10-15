//! Input to analysis.

mod lower_cm;
mod lower_mlb;
mod root_group;
mod topo;
mod util;

use fast_hash::FxHashMap;
use paths::{PathId, PathMap, WithPath};
use util::{ErrorKind, ErrorSource, GroupPathKind, GroupPathToProcess, Result, StartedGroupFile};

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
  pub(crate) severities: FxHashMap<diagnostic_util::Code, diagnostic_util::Severity>,
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
    let mut sources = PathMap::<String>::default();
    let mut groups = PathMap::<Group>::default();
    let init = GroupPathToProcess { parent: root_group.path, range: None, path: root_group.path };
    match root_group.kind {
      GroupPathKind::Cm => {
        let mut cm_files = PathMap::<lower_cm::CmFile>::default();
        lower_cm::get(fs, store, &root_group.config.path_vars, &mut sources, &mut cm_files, init)?;
        groups.extend(cm_files.into_iter().map(|(path, cm_file)| {
          let exports: Vec<_> = cm_file
            .exports
            .into_iter()
            .map(|ex| mlb_statics::BasDec::Export(ex.namespace, ex.name.clone(), ex.name))
            .collect();
          let paths: Vec<_> = std::iter::empty()
            .chain(
              cm_file
                .cm_paths
                .iter()
                .map(|&p| mlb_statics::BasDec::Path(p, mlb_statics::PathKind::Group)),
            )
            .chain(
              cm_file
                .sml_paths
                .iter()
                .map(|&p| mlb_statics::BasDec::Path(p, mlb_statics::PathKind::Source(None))),
            )
            .collect();
          let bas_dec = mlb_statics::BasDec::Local(
            mlb_statics::BasDec::seq(paths).into(),
            mlb_statics::BasDec::seq(exports).into(),
          );
          let group = Group { bas_dec, pos_db: cm_file.pos_db.expect("no pos db") };
          (path, group)
        }));
      }
      GroupPathKind::Mlb => {
        let mut stack = vec![init];
        while let Some(cur) = stack.pop() {
          if groups.contains_key(&cur.path) {
            continue;
          }
          let group_file = StartedGroupFile::new(store, cur, fs)?;
          let group_path = group_file.path.as_path();
          let group_parent = group_path.parent().expect("path from get_path has no parent");
          let syntax_dec =
            match mlb_syntax::get(group_file.contents.as_str(), &root_group.config.path_vars) {
              Ok(x) => x,
              Err(e) => {
                return Err(Error {
                  source: ErrorSource {
                    path: None,
                    range: group_file.pos_db.range(e.text_range()),
                  },
                  path: group_path.to_owned(),
                  kind: ErrorKind::Mlb(e),
                });
              }
            };
          let mut cx = lower_mlb::MlbCx {
            path: group_path,
            parent: group_parent,
            pos_db: &group_file.pos_db,
            fs,
            store,
            sources: &mut sources,
            stack: &mut stack,
            path_id: cur.path,
          };
          let bas_dec = lower_mlb::get_bas_dec(&mut cx, syntax_dec)?;
          groups.insert(cur.path, Group { bas_dec, pos_db: group_file.pos_db });
        }
      }
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
