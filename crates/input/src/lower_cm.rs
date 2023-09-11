//! Lower a CM file into paths and exports.

use crate::types::Group;
use crate::util::{
  get_path_id_in_group, read_file, Error, ErrorKind, ErrorSource, GroupPathToProcess, IoError,
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
  paths: &mut paths::Store,
  path_vars: &slash_var_path::Env,
  path: paths::PathId,
  errors: &mut Vec<Error>,
) where
  F: paths::FileSystem,
{
  let mut st = St { fs, paths, path_vars, sources, cm_files: PathMap::<CmFile>::default(), errors };
  let init = GroupPathToProcess { parent: path, range: None, path };
  if let Err(e) = get_one(&mut st, init) {
    st.errors.push(e.into_error());
  }
  for (path, cm_file) in st.cm_files {
    let exports: Vec<_> = cm_file
      .exports
      .into_iter()
      .map(|(ex, range)| {
        let name = WithRange { val: ex.name, range };
        mlb_hir::BasDec::Export(ex.namespace, name.clone(), name)
      })
      .collect();
    let path_decs: Vec<_> = cm_file
      .cm_paths
      .iter()
      .map(|&p| mlb_hir::BasDec::Path(p, mlb_hir::PathKind::Group))
      .chain(std::iter::once(mlb_hir::BasDec::SourcePathSet(cm_file.sml_paths)))
      .collect();
    let bas_dec = mlb_hir::BasDec::Local(
      mlb_hir::BasDec::seq(path_decs).into(),
      mlb_hir::BasDec::seq(exports).into(),
    );
    let group = Group { bas_dec, pos_db: cm_file.pos_db.expect("no pos db") };
    groups.insert(path, group);
  }
}

struct St<'a, F> {
  fs: &'a F,
  paths: &'a mut paths::Store,
  path_vars: &'a slash_var_path::Env,
  sources: &'a mut PathMap<String>,
  cm_files: PathMap<CmFile>,
  errors: &'a mut Vec<Error>,
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
  namespace: sml_namespace::Module,
  name: str_util::Name,
}

/// only recursive to support library exports, which ~necessitates the ability to know the exports
/// of a given library path on demand.
//
// iff this returns `Ok(())`, then `cur.path` is in `st.cm_files`.
fn get_one<F>(st: &mut St<'_, F>, cur: GroupPathToProcess) -> Result<(), IoError>
where
  F: paths::FileSystem,
{
  if st.cm_files.contains_key(&cur.path) {
    return Ok(());
  }
  // HACK: fake it so we don't infinitely recurse. this will be overwritten later.
  st.cm_files.insert(cur.path, CmFile::default());
  let mut ret = CmFile::default();
  let group = StartedGroup::new(st.paths, cur, st.fs)?;
  match cm_syntax::get(group.contents.as_str(), st.path_vars) {
    Ok(cm) => get_one_cm_file(st, &mut ret, cur.path, &group, cm),
    Err(e) => st.errors.push(Error::new(
      ErrorSource { path: None, range: group.pos_db.range_utf16(e.text_range()) },
      group.path.as_path().to_owned(),
      ErrorKind::Cm(e),
    )),
  }
  ret.pos_db = Some(group.pos_db);
  st.cm_files.insert(cur.path, ret);
  Ok(())
}

fn get_one_cm_file<F>(
  st: &mut St<'_, F>,
  ret: &mut CmFile,
  cur_path_id: paths::PathId,
  group: &StartedGroup,
  cm: cm_syntax::CmFile,
) where
  F: paths::FileSystem,
{
  for pp in cm.paths {
    let pid = get_path_id_in_group(st.fs, st.paths, group, pp.val.as_path(), pp.range);
    let (path_id, path, source) = match pid {
      Ok(x) => x,
      Err(e) => {
        st.errors.push(e);
        continue;
      }
    };
    match pp.val.kind() {
      cm_syntax::PathKind::Sml => {
        let contents = match read_file(st.fs, source, path.as_path()) {
          Ok(x) => x,
          Err(e) => {
            st.errors.push(e);
            continue;
          }
        };
        st.sources.insert(path_id, contents);
        ret.sml_paths.insert(path_id);
      }
      cm_syntax::PathKind::Cm => {
        let cur = GroupPathToProcess { parent: cur_path_id, range: source.range, path: path_id };
        match get_one(st, cur) {
          Ok(()) => ret.cm_paths.push(path_id),
          Err(e) => st.errors.push(e.into_error()),
        }
      }
    }
  }
  let cx = ExportCx { group, cm_paths: &ret.cm_paths, sml_paths: &ret.sml_paths, cur_path_id };
  if let cm_syntax::Export::Union(es) = &cm.export {
    if es.is_empty() {
      match cm.kind {
        cm_syntax::CmFileKind::Group => {}
        cm_syntax::CmFileKind::Library => {
          st.errors.push(Error::new(
            ErrorSource { path: None, range: cx.group.pos_db.range_utf16(cm.first_token_range) },
            cx.group.path.as_path().to_owned(),
            ErrorKind::LibraryEmptyExport,
          ));
        }
      }
    }
  }
  get_export(st, cx, &mut ret.exports, cm.export);
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
) where
  F: paths::FileSystem,
{
  match export {
    cm_syntax::Export::Name(ns, name) => {
      let namespace = match ns.val {
        cm_syntax::Namespace::Structure => sml_namespace::Module::Structure,
        cm_syntax::Namespace::Signature => sml_namespace::Module::Signature,
        cm_syntax::Namespace::Functor => sml_namespace::Module::Functor,
        cm_syntax::Namespace::FunSig => {
          st.errors.push(Error::new(
            ErrorSource { path: None, range: cx.group.pos_db.range_utf16(ns.range) },
            cx.group.path.as_path().to_owned(),
            ErrorKind::FunSig,
          ));
          return;
        }
      };
      ac.insert(NameExport { namespace, name: name.val }, name.range);
    }
    cm_syntax::Export::Library(lib) => {
      let p = match &lib.val {
        cm_syntax::PathOrStdBasis::Path(p) => p,
        cm_syntax::PathOrStdBasis::StdBasis => return,
      };
      get_one_and_extend_with(st, cx.group, cx.cur_path_id, p.as_path(), lib.range, ac);
    }
    cm_syntax::Export::Source(path) => match path.val {
      cm_syntax::PathOrMinus::Path(p) => {
        let pid = get_path_id_in_group(st.fs, st.paths, cx.group, p.as_path(), path.range);
        let (path_id, _, source) = match pid {
          Ok(x) => x,
          Err(e) => {
            st.errors.push(e);
            return;
          }
        };
        match st.sources.get(&path_id) {
          Some(contents) => get_top_defs(contents.as_str(), ac, path.range),
          None => st.errors.push(Error::new(
            source,
            cx.group.path.as_path().to_owned(),
            ErrorKind::SourcePathNotInFiles,
          )),
        }
      }
      cm_syntax::PathOrMinus::Minus => all_sources(st, cx, path.range, ac),
    },
    cm_syntax::Export::Group(path) => match path.val {
      cm_syntax::PathOrMinus::Path(p) => {
        get_one_and_extend_with(st, cx.group, cx.cur_path_id, p.as_path(), path.range, ac);
      }
      cm_syntax::PathOrMinus::Minus => all_groups(st, cx, path.range, ac),
    },
    cm_syntax::Export::Union(exports) => {
      for export in exports {
        get_export(st, cx, ac, export);
      }
    }
    cm_syntax::Export::Difference(lhs, rhs) => {
      let mut lhs_ac = NameExports::new();
      let mut rhs_ac = NameExports::new();
      get_export(st, cx, &mut lhs_ac, *lhs);
      get_export(st, cx, &mut rhs_ac, *rhs);
      // keep only those that ARE NOT in rhs.
      lhs_ac.retain(|k, _| !rhs_ac.contains_key(k));
      ac.extend(lhs_ac);
    }
    cm_syntax::Export::Intersection(lhs, rhs) => {
      let mut lhs_ac = NameExports::new();
      let mut rhs_ac = NameExports::new();
      get_export(st, cx, &mut lhs_ac, *lhs);
      get_export(st, cx, &mut rhs_ac, *rhs);
      // keep only those that ARE in rhs. only 1 character of difference from the Difference case!
      lhs_ac.retain(|k, _| rhs_ac.contains_key(k));
      ac.extend(lhs_ac);
    }
  }
}

fn all_sources<F>(st: &mut St<'_, F>, cx: ExportCx<'_>, range: TextRange, ac: &mut NameExports)
where
  F: paths::FileSystem,
{
  for path_id in cx.sml_paths {
    let contents = st.sources.get(path_id).expect("sml file should be set").as_str();
    get_top_defs(contents, ac, range);
  }
}

fn all_groups<F>(st: &mut St<'_, F>, cx: ExportCx<'_>, range: TextRange, ac: &mut NameExports)
where
  F: paths::FileSystem,
{
  for &path_id in cx.cm_paths {
    extend_with(st, path_id, range, ac);
  }
}

fn get_one_and_extend_with<F>(
  st: &mut St<'_, F>,
  group: &StartedGroup,
  parent: paths::PathId,
  path: &std::path::Path,
  range: TextRange,
  ac: &mut NameExports,
) where
  F: paths::FileSystem,
{
  let pid = get_path_id_in_group(st.fs, st.paths, group, path, range);
  let (path_id, _, source) = match pid {
    Ok(x) => x,
    Err(e) => {
      st.errors.push(e);
      return;
    }
  };
  let cur = GroupPathToProcess { parent, range: source.range, path: path_id };
  match get_one(st, cur) {
    Ok(()) => extend_with(st, cur.path, range, ac),
    Err(e) => st.errors.push(e.into_error()),
  }
}

fn extend_with<F>(st: &mut St<'_, F>, path: paths::PathId, range: TextRange, ac: &mut NameExports)
where
  F: paths::FileSystem,
{
  let other = st.cm_files.get(&path).expect("cm file should be set after successful get_one");
  ac.extend(other.exports.keys().map(|ex| (ex.clone(), range)));
}

/// it's pretty annoying to have to do this here, but not sure if there's a better option.
fn get_top_defs(contents: &str, ac: &mut NameExports, range: TextRange) {
  let mut fix_env = sml_fixity::STD_BASIS.clone();
  let (_, parse) = sml_file_syntax::SourceFileSyntax::lex_and_parse(&mut fix_env, contents);
  get_top_defs_dec(ac, parse.root.decs(), range);
}

fn get_top_defs_dec<I>(ac: &mut NameExports, iter: I, range: TextRange)
where
  I: Iterator<Item = sml_syntax::ast::Dec>,
{
  let iter = iter
    .filter_map(|x| x.dec_with_tail())
    .flat_map(|x| x.dec_in_seqs())
    .filter_map(|x| x.dec_one());
  for dec in iter {
    get_top_defs_dec_one(ac, dec, range);
  }
}

fn get_top_defs_dec_one(ac: &mut NameExports, dec: sml_syntax::ast::DecOne, range: TextRange) {
  match dec {
    sml_syntax::ast::DecOne::LocalDec(dec) => {
      let decs = dec.local_dec_tl().into_iter().flat_map(|x| x.decs());
      get_top_defs_dec(ac, decs, range);
    }
    sml_syntax::ast::DecOne::StructureDec(dec) => ac.extend(dec.str_binds().filter_map(|x| {
      let export = NameExport {
        namespace: sml_namespace::Module::Structure,
        name: str_util::Name::new(x.name()?.text()),
      };
      Some((export, range))
    })),
    sml_syntax::ast::DecOne::SignatureDec(dec) => ac.extend(dec.sig_binds().filter_map(|x| {
      let export = NameExport {
        namespace: sml_namespace::Module::Signature,
        name: str_util::Name::new(x.name()?.text()),
      };
      Some((export, range))
    })),
    sml_syntax::ast::DecOne::FunctorDec(dec) => ac.extend(dec.functor_binds().filter_map(|x| {
      let export = NameExport {
        namespace: sml_namespace::Module::Functor,
        name: str_util::Name::new(x.functor_name()?.text()),
      };
      Some((export, range))
    })),
    _ => {}
  }
}
