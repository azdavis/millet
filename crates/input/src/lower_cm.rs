//! Lower a CM file into paths and exports.

use crate::types::Group;
use crate::util::{
  get_path_id_in_group, read_file, Error, ErrorKind, ErrorSource, GroupPathToProcess, Result,
  StartedGroup,
};
use fast_hash::FxHashSet;
use paths::PathMap;
use std::collections::BTreeMap;
use text_size_util::{TextRange, WithRange};

pub(crate) fn get<F>(
  fs: &F,
  sources: &mut PathMap<String>,
  groups: &mut PathMap<Group>,
  store: &mut paths::Store,
  path_vars: &paths::slash_var_path::Env,
  path: paths::PathId,
) -> Result<()>
where
  F: paths::FileSystem,
{
  let mut st = St { fs, store, path_vars, sources, cm_files: PathMap::<CmFile>::default() };
  let init = GroupPathToProcess { parent: path, range: None, path };
  get_one(&mut st, init)?;
  for (path, cm_file) in st.cm_files {
    let exports: Vec<_> = cm_file
      .exports
      .into_iter()
      .map(|(ex, range)| {
        let name = WithRange { val: ex.name, range };
        mlb_statics::BasDec::Export(ex.namespace, name.clone(), name)
      })
      .collect();
    let path_decs: Vec<_> = cm_file
      .cm_paths
      .iter()
      .map(|&p| mlb_statics::BasDec::Path(p, mlb_statics::PathKind::Group))
      .chain(std::iter::once(mlb_statics::BasDec::SourcePathSet(cm_file.sml_paths)))
      .collect();
    let bas_dec = mlb_statics::BasDec::Local(
      mlb_statics::BasDec::seq(path_decs).into(),
      mlb_statics::BasDec::seq(exports).into(),
    );
    let group = Group { bas_dec, pos_db: cm_file.pos_db.expect("no pos db") };
    groups.insert(path, group);
  }
  Ok(())
}

struct St<'a, F> {
  fs: &'a F,
  store: &'a mut paths::Store,
  path_vars: &'a paths::slash_var_path::Env,
  sources: &'a mut PathMap<String>,
  cm_files: PathMap<CmFile>,
}

/// only derives default because we need to mark in-progress files as visited to prevent infinite
/// recursing.
#[derive(Debug, Default)]
struct CmFile {
  /// only optional so this can derive default.
  pos_db: Option<text_pos::PositionDb>,
  cm_paths: Vec<paths::PathId>,
  sml_paths: FxHashSet<paths::PathId>,
  exports: NameExports,
}

type NameExports = BTreeMap<NameExport, TextRange>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct NameExport {
  namespace: sml_statics::basis::Namespace,
  name: str_util::Name,
}

/// only recursive to support library exports, which ~necessitates the ability to know the exports
/// of a given library path on demand.
fn get_one<F>(st: &mut St<'_, F>, cur: GroupPathToProcess) -> Result<()>
where
  F: paths::FileSystem,
{
  if st.cm_files.contains_key(&cur.path) {
    return Ok(());
  }
  // HACK: fake it so we don't infinitely recurse. this will be overwritten later.
  st.cm_files.insert(cur.path, CmFile::default());
  let mut ret = CmFile::default();
  let group = StartedGroup::new(st.store, cur, st.fs)?;
  let cm = match cm_syntax::get(group.contents.as_str(), st.path_vars) {
    Ok(x) => x,
    Err(e) => {
      return Err(Error::new(
        ErrorSource { path: None, range: group.pos_db.range(e.text_range()) },
        group.path.as_path().to_owned(),
        ErrorKind::Cm(e),
      ))
    }
  };
  for pp in cm.paths {
    let (path_id, path, source) =
      get_path_id_in_group(st.fs, st.store, &group, pp.val.as_path(), pp.range)?;
    match pp.val.kind() {
      cm_syntax::PathKind::Sml => {
        let contents = read_file(st.fs, source, path.as_path())?;
        st.sources.insert(path_id, contents);
        ret.sml_paths.insert(path_id);
      }
      cm_syntax::PathKind::Cm => {
        let cur = GroupPathToProcess { parent: cur.path, range: source.range, path: path_id };
        get_one(st, cur)?;
        ret.cm_paths.push(path_id);
      }
    }
  }
  let cx = ExportCx {
    group: &group,
    cm_paths: &ret.cm_paths,
    sml_paths: &ret.sml_paths,
    cur_path_id: cur.path,
  };
  get_export(st, cx, &mut ret.exports, cm.export)?;
  ret.pos_db = Some(group.pos_db);
  st.cm_files.insert(cur.path, ret);
  Ok(())
}

#[derive(Clone, Copy)]
struct ExportCx<'a> {
  group: &'a StartedGroup,
  cm_paths: &'a [paths::PathId],
  sml_paths: &'a FxHashSet<paths::PathId>,
  cur_path_id: paths::PathId,
}

fn get_export<F>(
  st: &mut St<'_, F>,
  cx: ExportCx<'_>,
  ac: &mut NameExports,
  export: cm_syntax::Export,
) -> Result<()>
where
  F: paths::FileSystem,
{
  match export {
    cm_syntax::Export::Name(ns, name) => {
      let namespace = match ns.val {
        cm_syntax::Namespace::Structure => sml_statics::basis::Namespace::Structure,
        cm_syntax::Namespace::Signature => sml_statics::basis::Namespace::Signature,
        cm_syntax::Namespace::Functor => sml_statics::basis::Namespace::Functor,
        cm_syntax::Namespace::FunSig => {
          return Err(Error::new(
            ErrorSource { path: None, range: cx.group.pos_db.range(ns.range) },
            cx.group.path.as_path().to_owned(),
            ErrorKind::FunSig,
          ))
        }
      };
      ac.insert(NameExport { namespace, name: name.val }, name.range);
    }
    cm_syntax::Export::Library(lib) => {
      let p = match &lib.val {
        cm_syntax::PathOrStdBasis::Path(p) => p,
        cm_syntax::PathOrStdBasis::StdBasis => return Ok(()),
      };
      get_one_and_extend_with(st, cx.group, cx.cur_path_id, p.as_path(), lib.range, ac)?;
    }
    cm_syntax::Export::Source(path) => match path.val {
      cm_syntax::PathOrMinus::Path(p) => {
        let (path_id, _, source) =
          get_path_id_in_group(st.fs, st.store, cx.group, p.as_path(), path.range)?;
        let contents = match st.sources.get(&path_id) {
          Some(x) => x.as_str(),
          None => {
            return Err(Error::new(
              source,
              cx.group.path.as_path().to_owned(),
              ErrorKind::SourcePathNotInFiles,
            ))
          }
        };
        get_top_defs(contents, ac, path.range);
      }
      cm_syntax::PathOrMinus::Minus => {
        for path_id in cx.sml_paths {
          let contents = st.sources.get(path_id).expect("sml file should be set").as_str();
          get_top_defs(contents, ac, path.range);
        }
      }
    },
    cm_syntax::Export::Group(path) => match path.val {
      cm_syntax::PathOrMinus::Path(p) => {
        get_one_and_extend_with(st, cx.group, cx.cur_path_id, p.as_path(), path.range, ac)?;
      }
      cm_syntax::PathOrMinus::Minus => {
        for &path_id in cx.cm_paths {
          extend_with(st, path_id, path.range, ac);
        }
      }
    },
    cm_syntax::Export::Union(exports) => {
      for export in exports {
        get_export(st, cx, ac, export)?;
      }
    }
    cm_syntax::Export::Difference(lhs, rhs) => {
      let mut lhs_ac = NameExports::new();
      let mut rhs_ac = NameExports::new();
      get_export(st, cx, &mut lhs_ac, *lhs)?;
      get_export(st, cx, &mut rhs_ac, *rhs)?;
      // keep only those that ARE NOT in rhs.
      lhs_ac.retain(|k, _| !rhs_ac.contains_key(k));
      ac.extend(lhs_ac);
    }
    cm_syntax::Export::Intersection(lhs, rhs) => {
      let mut lhs_ac = NameExports::new();
      let mut rhs_ac = NameExports::new();
      get_export(st, cx, &mut lhs_ac, *lhs)?;
      get_export(st, cx, &mut rhs_ac, *rhs)?;
      // keep only those that ARE in rhs. only 1 character of difference from the Difference case!
      lhs_ac.retain(|k, _| rhs_ac.contains_key(k));
      ac.extend(lhs_ac);
    }
  }
  Ok(())
}

fn get_one_and_extend_with<F>(
  st: &mut St<'_, F>,
  group: &StartedGroup,
  parent: paths::PathId,
  path: &std::path::Path,
  range: TextRange,
  ac: &mut NameExports,
) -> Result<()>
where
  F: paths::FileSystem,
{
  let (path_id, _, source) = get_path_id_in_group(st.fs, st.store, group, path, range)?;
  let cur = GroupPathToProcess { parent, range: source.range, path: path_id };
  get_one(st, cur)?;
  extend_with(st, cur.path, range, ac);
  Ok(())
}

fn extend_with<F>(st: &mut St<'_, F>, path: paths::PathId, range: TextRange, ac: &mut NameExports)
where
  F: paths::FileSystem,
{
  let other = st.cm_files.get(&path).expect("cm file should be set after get");
  ac.extend(other.exports.keys().map(|ex| (ex.clone(), range)));
}

/// it's pretty annoying to have to do this here, but not sure if there's a better option.
fn get_top_defs(contents: &str, ac: &mut NameExports, range: TextRange) {
  let mut fix_env = sml_parse::parser::STD_BASIS.clone();
  let (_, parse) = mlb_statics::SourceFileSyntax::lex_and_parse(&mut fix_env, contents);
  get_top_defs_dec(ac, parse.root.dec(), range);
}

fn get_top_defs_dec(ac: &mut NameExports, dec: Option<sml_syntax::ast::Dec>, range: TextRange) {
  let iter = dec
    .into_iter()
    .flat_map(|x| x.dec_with_tail_in_seqs())
    .filter_map(|x| x.dec_with_tail())
    .flat_map(|x| x.dec_in_seqs())
    .filter_map(|x| x.dec_one());
  for dec in iter {
    match dec {
      sml_syntax::ast::DecOne::LocalDec(dec) => {
        get_top_defs_dec(ac, dec.in_dec(), range);
      }
      sml_syntax::ast::DecOne::StructureDec(dec) => {
        ac.extend(dec.str_binds().filter_map(|x| {
          let export = NameExport {
            namespace: sml_statics::basis::Namespace::Structure,
            name: str_util::Name::new(x.name()?.text()),
          };
          Some((export, range))
        }));
      }
      sml_syntax::ast::DecOne::SignatureDec(dec) => {
        ac.extend(dec.sig_binds().filter_map(|x| {
          let export = NameExport {
            namespace: sml_statics::basis::Namespace::Signature,
            name: str_util::Name::new(x.name()?.text()),
          };
          Some((export, range))
        }));
      }
      sml_syntax::ast::DecOne::FunctorDec(dec) => {
        ac.extend(dec.functor_binds().filter_map(|x| {
          let export = NameExport {
            namespace: sml_statics::basis::Namespace::Functor,
            name: str_util::Name::new(x.functor_name()?.text()),
          };
          Some((export, range))
        }));
      }
      _ => {}
    }
  }
}
