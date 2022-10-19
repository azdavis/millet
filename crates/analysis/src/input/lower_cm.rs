//! Lower a CM file into paths and exports.

use crate::input::util::{
  get_path_id, read_file, Error, ErrorKind, ErrorSource, GroupPathToProcess, Result,
  StartedGroupFile,
};
use fast_hash::FxHashSet;

/// only derives default because we need to mark in-progress files as visited to prevent infinite
/// recursing.
#[derive(Debug, Default)]
pub(crate) struct CmFile {
  /// only optional so this can derive default.
  pub(crate) pos_db: Option<text_pos::PositionDb>,
  pub(crate) cm_paths: Vec<paths::PathId>,
  pub(crate) sml_paths: FxHashSet<paths::PathId>,
  pub(crate) exports: Vec<Export>,
}

#[derive(Debug, Clone)]
pub(crate) struct Export {
  pub(crate) namespace: sml_statics::basis::Namespace,
  pub(crate) name: text_size_util::WithRange<str_util::Name>,
}

/// only recursive to support library exports, which ~necessitates the ability to know the exports
/// of a given library path on demand.
#[allow(clippy::too_many_lines)]
pub(crate) fn get<F>(
  fs: &F,
  store: &mut paths::Store,
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
  let mut ret = CmFile::default();
  let group_file = StartedGroupFile::new(store, cur, fs)?;
  let group_path = group_file.path.as_path();
  let group_parent = group_path.parent().expect("path from get_path has no parent");
  let cm = match cm_syntax::get(group_file.contents.as_str(), path_vars) {
    Ok(x) => x,
    Err(e) => {
      return Err(Error {
        source: ErrorSource { path: None, range: group_file.pos_db.range(e.text_range()) },
        path: group_path.to_owned(),
        kind: ErrorKind::Cm(e),
      })
    }
  };
  for parsed_path in cm.paths {
    let source = ErrorSource {
      path: Some(group_path.to_owned()),
      range: group_file.pos_db.range(parsed_path.range),
    };
    let path = group_parent.join(parsed_path.val.as_path());
    let path_id = get_path_id(fs, store, source.clone(), path.as_path())?;
    match parsed_path.val.kind() {
      cm_syntax::PathKind::Sml => {
        let contents = read_file(fs, source, path.as_path())?;
        sources.insert(path_id, contents);
        ret.sml_paths.insert(path_id);
      }
      cm_syntax::PathKind::Cm => {
        let cur = GroupPathToProcess { parent: cur.path, range: source.range, path: path_id };
        get(fs, store, path_vars, sources, cm_files, cur)?;
        ret.cm_paths.push(path_id);
      }
    }
  }
  for export in cm.exports {
    match export {
      cm_syntax::Export::Name(ns, name) => {
        let namespace = match ns.val {
          cm_syntax::Namespace::Structure => sml_statics::basis::Namespace::Structure,
          cm_syntax::Namespace::Signature => sml_statics::basis::Namespace::Signature,
          cm_syntax::Namespace::Functor => sml_statics::basis::Namespace::Functor,
          cm_syntax::Namespace::FunSig => {
            return Err(Error {
              source: ErrorSource { path: None, range: group_file.pos_db.range(ns.range) },
              path: group_path.to_owned(),
              kind: ErrorKind::UnsupportedExport,
            })
          }
        };
        ret.exports.push(Export { namespace, name });
      }
      cm_syntax::Export::Library(lib) => {
        let source = ErrorSource {
          path: Some(group_path.to_owned()),
          range: group_file.pos_db.range(lib.range),
        };
        let path = match &lib.val {
          cm_syntax::PathOrStdBasis::Path(p) => p.as_path(),
          cm_syntax::PathOrStdBasis::StdBasis => continue,
        };
        let path = group_parent.join(path);
        let path_id = get_path_id(fs, store, source.clone(), path.as_path())?;
        let cur = GroupPathToProcess { parent: cur.path, range: source.range, path: path_id };
        get(fs, store, path_vars, sources, cm_files, cur)?;
        let other = cm_files.get(&cur.path).expect("cm file should be set after get");
        ret.exports.extend(
          other
            .exports
            .iter()
            .map(|ex| Export { namespace: ex.namespace, name: lib.wrap(ex.name.val.clone()) }),
        );
      }
      cm_syntax::Export::Source(path) => {
        return Err(Error {
          source: ErrorSource { path: None, range: group_file.pos_db.range(path.range) },
          path: group_path.to_owned(),
          kind: ErrorKind::UnsupportedExport,
        })
      }
      cm_syntax::Export::Group(path) => match path.val {
        cm_syntax::PathOrMinus::Path(p) => {
          let source = ErrorSource {
            path: Some(group_path.to_owned()),
            range: group_file.pos_db.range(path.range),
          };
          let path = group_parent.join(p.as_path());
          let path_id = get_path_id(fs, store, source.clone(), path.as_path())?;
          let cur = GroupPathToProcess { parent: cur.path, range: source.range, path: path_id };
          get(fs, store, path_vars, sources, cm_files, cur)?;
          let other = cm_files.get(&cur.path).expect("cm file should be set after get");
          ret.exports.extend(other.exports.iter().cloned());
        }
        cm_syntax::PathOrMinus::Minus => {
          for path in &ret.cm_paths {
            let other = cm_files.get(path).expect("cm file should be set after get");
            ret.exports.extend(other.exports.iter().cloned());
          }
        }
      },
    }
  }
  ret.pos_db = Some(group_file.pos_db);
  cm_files.insert(cur.path, ret);
  Ok(())
}
