//! Input to analysis.

mod lower_cm;
mod lower_mlb;
mod root;
mod topo;
mod util;

use paths::{PathId, PathMap, WithPath};
use util::{
  ErrorSource, GetInputErrorKind, GroupPathKind, GroupPathToProcess, Result, StartedGroupFile,
};

pub use util::InputError;

/// The input to analysis.
#[derive(Debug)]
pub struct Input {
  /// A map from source paths to their contents.
  pub(crate) sources: PathMap<String>,
  /// A map from group paths to their (parsed) contents.
  pub(crate) groups: PathMap<Group>,
  /// The root group id.
  pub(crate) root_group_id: PathId,
}

impl Input {
  /// Get input anchored at the root.
  pub fn new<F>(fs: &F, root: &mut paths::Root) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let root_group = root::Root::new(fs, root)?;
    let init = GroupPathToProcess { parent: root_group.path, range: None, path: root_group.path };
    let mut sources = PathMap::<String>::default();
    let groups = match root_group.kind {
      GroupPathKind::Cm => {
        let mut cm_files = PathMap::<lower_cm::CmFile>::default();
        lower_cm::get(root, fs, &root_group.path_vars, &mut sources, &mut cm_files, init)?;
        cm_files
          .into_iter()
          .map(|(path, cm_file)| {
            let exports: Vec<_> = cm_file
              .exports
              .into_iter()
              .map(|ex| mlb_hir::BasDec::Export(ex.namespace, ex.name.clone(), ex.name))
              .collect();
            let bas_dec = mlb_hir::BasDec::Local(
              mlb_hir::BasDec::seq(cm_file.paths).into(),
              mlb_hir::BasDec::seq(exports).into(),
            );
            let group = Group { bas_dec, pos_db: cm_file.pos_db.expect("no pos db") };
            (path, group)
          })
          .collect()
      }
      GroupPathKind::Mlb => {
        let mut groups = PathMap::<Group>::default();
        let mut stack = vec![init];
        while let Some(cur) = stack.pop() {
          if groups.contains_key(&cur.path) {
            continue;
          }
          let group_file = StartedGroupFile::new(root, cur, fs)?;
          let group_path = group_file.path.as_path();
          let group_parent = group_path.parent().expect("path from get_path has no parent");
          let syntax_dec = mlb_syntax::get(group_file.contents.as_str(), &root_group.path_vars)
            .map_err(|e| InputError {
              source: ErrorSource { path: None, range: group_file.pos_db.range(e.text_range()) },
              path: group_path.to_owned(),
              kind: GetInputErrorKind::Mlb(e),
            })?;
          let mut cx = lower_mlb::MlbCx {
            path: group_path,
            parent: group_parent,
            pos_db: &group_file.pos_db,
            fs,
            root,
            sources: &mut sources,
            stack: &mut stack,
            path_id: cur.path,
          };
          let bas_dec = lower_mlb::get_bas_dec(&mut cx, syntax_dec)?;
          groups.insert(cur.path, Group { bas_dec, pos_db: group_file.pos_db });
        }
        groups
      }
    };
    let bas_decs = groups.iter().map(|(&a, b)| (a, &b.bas_dec));
    if let Err(err) = topo::check(bas_decs) {
      return Err(InputError {
        source: ErrorSource::default(),
        path: root.get_path(err.witness()).as_path().to_owned(),
        kind: GetInputErrorKind::Cycle,
      });
    }
    Ok(Self { sources, groups, root_group_id: root_group.path })
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
  pub(crate) bas_dec: mlb_hir::BasDec,
  /// A position DB for the group file that yielded the dec.
  pub(crate) pos_db: text_pos::PositionDb,
}
