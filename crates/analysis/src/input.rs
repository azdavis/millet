//! Input to analysis.

mod group_path;
mod lower_cm;
mod lower_mlb;
mod topo;
mod util;

use paths::{PathId, PathMap, WithPath};
use std::path::Path;
use util::{
  canonicalize, start_group_file, ErrorSource, GetInputErrorKind, GroupPathToProcess, Result,
};

pub use group_path::{get_root_dir, Root};
pub use util::GetInputError;

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
  /// Return an iterator over the source paths.
  pub fn iter_sources(&self) -> impl Iterator<Item = WithPath<&str>> + '_ {
    self.sources.iter().map(|(&path, s)| path.wrap(s.as_str()))
  }

  /// Override a source file to have the given contents.
  ///
  /// Returns whether the source was overridden. That is:
  ///
  /// - This returns `true` if there _was_ an existing source with this `path`.
  /// - This returns `false` otherwise.
  pub fn override_source(&mut self, path: PathId, contents: String) -> bool {
    self.sources.insert(path, contents).is_some()
  }
}

#[derive(Debug)]
pub(crate) struct Group {
  pub(crate) bas_dec: mlb_hir::BasDec,
  pub(crate) pos_db: text_pos::PositionDb,
}

/// Given a path to either a group path or a directory, return the root for it.
pub fn get_root<F>(fs: &F, path: &Path) -> Result<Root>
where
  F: paths::FileSystem,
{
  let path = canonicalize(fs, path, &ErrorSource::default())?;
  let (root_path, group_path) = match group_path::GroupPath::new(fs, path.clone().into_path_buf()) {
    None => (path, None),
    Some(path) => {
      let parent = path.as_path().parent().expect("no parent");
      let rp = fs.canonicalize(parent).expect("canonicalize parent of canonical path");
      (rp, Some(path))
    }
  };
  Ok(Root::new(paths::Root::new(root_path), group_path))
}

/// Get some input from the filesystem.
pub fn get<F>(fs: &F, root: &mut Root) -> Result<Input>
where
  F: paths::FileSystem,
{
  let root_group = group_path::get_root_group_path(fs, root)?;
  let init = GroupPathToProcess { parent: root_group.path, range: None, path: root_group.path };
  let mut sources = PathMap::<String>::default();
  let groups = match root_group.kind {
    group_path::GroupPathKind::Cm => {
      let mut cm_files = PathMap::<lower_cm::CmFile>::default();
      lower_cm::get(
        root.as_mut_paths(),
        fs,
        &root_group.path_vars,
        &mut sources,
        &mut cm_files,
        init,
      )?;
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
    group_path::GroupPathKind::Mlb => {
      let mut groups = PathMap::<Group>::default();
      let mut stack = vec![init];
      while let Some(cur) = stack.pop() {
        if groups.contains_key(&cur.path) {
          continue;
        }
        let (group_path, contents, pos_db) = start_group_file(root.as_mut_paths(), cur, fs)?;
        let group_path = group_path.as_path();
        let group_parent = group_path.parent().expect("path from get_path has no parent");
        let syntax_dec =
          mlb_syntax::get(&contents, &root_group.path_vars).map_err(|e| GetInputError {
            source: ErrorSource { path: None, range: pos_db.range(e.text_range()) },
            path: group_path.to_owned(),
            kind: GetInputErrorKind::Mlb(e),
          })?;
        let mut cx = lower_mlb::MlbCx {
          path: group_path,
          parent: group_parent,
          pos_db: &pos_db,
          fs,
          root: root.as_mut_paths(),
          sources: &mut sources,
          stack: &mut stack,
          path_id: cur.path,
        };
        let bas_dec = lower_mlb::get_bas_dec(&mut cx, syntax_dec)?;
        groups.insert(cur.path, Group { bas_dec, pos_db });
      }
      groups
    }
  };
  let bas_decs = groups.iter().map(|(&a, b)| (a, &b.bas_dec));
  if let Err(err) = topo::check(bas_decs) {
    return Err(GetInputError {
      source: ErrorSource::default(),
      path: root.as_paths().get_path(err.witness()).as_path().to_owned(),
      kind: GetInputErrorKind::Cycle,
    });
  }
  Ok(Input { sources, groups, root_group_id: root_group.path })
}
