//! Lower a CM file into paths and exports.

use crate::input::util::{
  get_path_id, read_file, ErrorKind, ErrorSource, GroupPathToProcess, InputError, Result,
  StartedGroupFile,
};

/// only derives default because we need to mark in-progress files as visited to prevent infinite
/// recursing.
#[derive(Debug, Default)]
pub(crate) struct CmFile {
  /// only optional so this can derive default.
  pub(crate) pos_db: Option<text_pos::PositionDb>,
  pub(crate) paths: Vec<mlb_hir::BasDec>,
  pub(crate) exports: Vec<Export>,
}

#[derive(Debug, Clone)]
pub(crate) struct Export {
  pub(crate) namespace: mlb_hir::Namespace,
  pub(crate) name: text_size_util::WithRange<str_util::Name>,
}

/// only recursive to support library exports, which ~necessitates the ability to know the exports
/// of a given library path on demand.
pub(crate) fn get<F>(
  store: &mut paths::Store,
  fs: &F,
  path_vars: &paths::slash_var_path::Env,
  sources: &mut paths::PathMap<String>,
  cm_files: &mut paths::PathMap<CmFile>,
  cur: GroupPathToProcess,
) -> Result<()>
where
  F: paths::FileSystem,
{
  if cm_files.contains_key(&cur.path) {
    return Ok(());
  }
  // HACK: fake it so we don't infinitely recurse. this will be overwritten later.
  cm_files.insert(cur.path, CmFile::default());
  let group_file = StartedGroupFile::new(store, cur, fs)?;
  let group_path = group_file.path.as_path();
  let group_parent = group_path.parent().expect("path from get_path has no parent");
  let cm = match cm::get(group_file.contents.as_str(), path_vars) {
    Ok(x) => x,
    Err(e) => {
      return Err(InputError {
        source: ErrorSource { path: None, range: group_file.pos_db.range(e.text_range()) },
        path: group_path.to_owned(),
        kind: ErrorKind::Cm(e),
      })
    }
  };
  let paths = cm
    .paths
    .into_iter()
    .map(|parsed_path| {
      let source = ErrorSource {
        path: Some(group_path.to_owned()),
        range: group_file.pos_db.range(parsed_path.range),
      };
      let path = group_parent.join(parsed_path.val.as_path());
      let path_id = get_path_id(fs, store, source.clone(), path.as_path())?;
      let kind = match parsed_path.val.kind() {
        cm::PathKind::Sml => {
          let contents = read_file(fs, source, path.as_path())?;
          sources.insert(path_id, contents);
          mlb_hir::PathKind::Sml
        }
        cm::PathKind::Cm => {
          let cur = GroupPathToProcess { parent: cur.path, range: source.range, path: path_id };
          get(store, fs, path_vars, sources, cm_files, cur)?;
          // NOTE this is a lie.
          mlb_hir::PathKind::Mlb
        }
      };
      Ok((path_id, kind))
    })
    .collect::<Result<Vec<_>>>()?;
  let mut exports = Vec::<Export>::new();
  for export in cm.exports {
    match export {
      cm::Export::Regular(ns, name) => {
        let namespace = match ns.val {
          cm::Namespace::Structure => mlb_hir::Namespace::Structure,
          cm::Namespace::Signature => mlb_hir::Namespace::Signature,
          cm::Namespace::Functor => mlb_hir::Namespace::Functor,
          cm::Namespace::FunSig => {
            return Err(InputError {
              source: ErrorSource { path: None, range: group_file.pos_db.range(ns.range) },
              path: group_path.to_owned(),
              kind: ErrorKind::UnsupportedExport,
            })
          }
        };
        exports.push(Export { namespace, name });
      }
      cm::Export::Library(lib) => {
        let source = ErrorSource {
          path: Some(group_path.to_owned()),
          range: group_file.pos_db.range(lib.range),
        };
        let path = group_parent.join(lib.val.as_path());
        let path_id = get_path_id(fs, store, source.clone(), path.as_path())?;
        let cur = GroupPathToProcess { parent: cur.path, range: source.range, path: path_id };
        get(store, fs, path_vars, sources, cm_files, cur)?;
        let cm_file = cm_files.get(&cur.path).expect("cm file should be set after get_cm_file");
        exports.extend(
          cm_file
            .exports
            .iter()
            .map(|ex| Export { namespace: ex.namespace, name: lib.wrap(ex.name.val.clone()) }),
        );
      }
      cm::Export::Source(range) => {
        return Err(InputError {
          source: ErrorSource { path: None, range: group_file.pos_db.range(range) },
          path: group_path.to_owned(),
          kind: ErrorKind::UnsupportedExport,
        })
      }
      cm::Export::Group(_) => {
        for (path, kind) in paths.iter() {
          if matches!(kind, mlb_hir::PathKind::Mlb) {
            let cm_file = cm_files.get(path).expect("child cm file should be set");
            exports.extend(cm_file.exports.iter().cloned());
          }
        }
      }
    }
  }
  let cm_file = CmFile {
    pos_db: Some(group_file.pos_db),
    paths: paths.into_iter().map(|(p, k)| mlb_hir::BasDec::Path(p, k)).collect(),
    exports,
  };
  cm_files.insert(cur.path, cm_file);
  Ok(())
}
