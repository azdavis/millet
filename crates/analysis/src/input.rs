//! Input to analysis.

use fast_hash::FxHashSet;
use paths::{PathId, PathMap, WithPath};
use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};
use text_pos::Range;

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

/// An error when getting input.
///
/// We might like this to be non-pub and just a variant of [`crate::error::Error`]. The problem is
/// that _sometimes_, a `GetInputError` is for a file which doesn't exist (or a non-file), so we
/// can't always show these in the editor inline.
#[derive(Debug)]
pub struct GetInputError {
  source: Source,
  path: PathBuf,
  kind: GetInputErrorKind,
}

impl GetInputError {
  /// Returns a path associated with this error, which may or may not exist.
  pub fn path(&self) -> &Path {
    self.source.path.as_ref().unwrap_or(&self.path).as_path()
  }

  /// Returns a range for this error in `path`.
  pub fn range(&self) -> Option<Range> {
    self.source.range
  }

  /// Returns the error code for this.
  pub fn to_code(&self) -> u16 {
    match self.kind {
      GetInputErrorKind::Io(_) => 1001,
      GetInputErrorKind::NotInRoot(_) => 1002,
      GetInputErrorKind::MultipleRoots(_, _) => 1003,
      GetInputErrorKind::NoRoot => 1004,
      GetInputErrorKind::NotGroup => 1005,
      GetInputErrorKind::CouldNotParseConfig(_) => 1006,
      GetInputErrorKind::InvalidConfigVersion(_) => 1007,
      GetInputErrorKind::Cm(_) => 1008,
      GetInputErrorKind::Mlb(_) => 1009,
      GetInputErrorKind::Cycle => 1010,
      GetInputErrorKind::Duplicate(_) => 1011,
      GetInputErrorKind::UnsupportedExport => 1999,
    }
  }
}

impl fmt::Display for GetInputError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.source.path.is_some() {
      write!(f, "{}: ", self.path.display())?;
    }
    self.kind.fmt(f)
  }
}

#[derive(Debug)]
enum GetInputErrorKind {
  Io(std::io::Error),
  NotInRoot(std::path::StripPrefixError),
  MultipleRoots(PathBuf, PathBuf),
  NoRoot,
  NotGroup,
  CouldNotParseConfig(toml::de::Error),
  InvalidConfigVersion(u16),
  Cm(cm::Error),
  Mlb(mlb_syntax::Error),
  Cycle,
  Duplicate(sml_hir::Name),
  /// must be last
  UnsupportedExport,
}

impl fmt::Display for GetInputErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      GetInputErrorKind::Io(e) => write!(f, "couldn't perform file I/O: {e}"),
      GetInputErrorKind::NotInRoot(e) => write!(f, "path not contained in root: {e}"),
      GetInputErrorKind::MultipleRoots(a, b) => {
        write!(f, "multiple root group files: {} and {}", a.display(), b.display())
      }
      GetInputErrorKind::NoRoot => f.write_str("no root group file"),
      GetInputErrorKind::NotGroup => f.write_str("not a group file path"),
      GetInputErrorKind::CouldNotParseConfig(e) => write!(f, "couldn't parse config: {e}"),
      GetInputErrorKind::InvalidConfigVersion(n) => {
        write!(f, "invalid config version: expected 1, found {n}")
      }
      GetInputErrorKind::Cm(e) => write!(f, "couldn't process SML/NJ CM file: {e}"),
      GetInputErrorKind::Mlb(e) => write!(f, "couldn't process ML Basis file: {e}"),
      GetInputErrorKind::Cycle => f.write_str("there is a cycle involving this path"),
      GetInputErrorKind::Duplicate(name) => write!(f, "duplicate name: {name}"),
      GetInputErrorKind::UnsupportedExport => f.write_str("unsupported export kind"),
    }
  }
}

#[derive(Debug, Default, Clone)]
struct Source {
  path: Option<PathBuf>,
  range: Option<Range>,
}

/// std's Result with GetInputError as the default error.
pub type Result<T, E = GetInputError> = std::result::Result<T, E>;

/// A kind of group path.
#[derive(Debug, Clone, Copy)]
enum GroupPathKind {
  /// SML/NJ Compilation Manager files.
  Cm,
  /// ML Basis files.
  Mlb,
}

/// A group path.
#[derive(Debug)]
struct GroupPath {
  kind: GroupPathKind,
  path: PathBuf,
}

impl GroupPath {
  /// Returns a new `GroupPath`.
  fn new<F>(fs: &F, path: PathBuf) -> Option<GroupPath>
  where
    F: paths::FileSystem,
  {
    if !fs.is_file(path.as_path()) {
      return None;
    }
    let kind = match path.extension()?.to_str()? {
      "cm" => GroupPathKind::Cm,
      "mlb" => GroupPathKind::Mlb,
      _ => return None,
    };
    Some(GroupPath { path, kind })
  }

  /// Return this as a `Path`.
  fn as_path(&self) -> &Path {
    self.path.as_path()
  }
}

#[derive(Debug)]
pub(crate) struct Group {
  pub(crate) bas_dec: mlb_hir::BasDec,
  pub(crate) pos_db: text_pos::PositionDb,
}

struct RootGroup {
  path: PathId,
  kind: GroupPathKind,
  path_vars: paths::slash_var_path::Env,
}

fn get_root_group<F>(fs: &F, root: &mut Root) -> Result<RootGroup>
where
  F: paths::FileSystem,
{
  let mut root_group_source = Source::default();
  let config_path = root.paths.as_path().join(config::FILE_NAME);
  let mut path_vars = paths::slash_var_path::Env::default();
  if let Ok(contents) = fs.read_to_string(&config_path) {
    let config: config::Root = match toml::from_str(&contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(GetInputError {
          source: Source::default(),
          path: config_path,
          kind: GetInputErrorKind::CouldNotParseConfig(e),
        })
      }
    };
    if config.version != 1 {
      return Err(GetInputError {
        source: Source::default(),
        path: config_path,
        kind: GetInputErrorKind::InvalidConfigVersion(config.version),
      });
    }
    if let Some(ws) = config.workspace {
      if let Some(ws_path_vars) = ws.path_vars {
        for (key, val) in ws_path_vars {
          match val {
            config::PathVar::Value(val) => {
              path_vars.insert(key, val);
            }
            config::PathVar::Path(p) => {
              let val: str_util::SmolStr =
                root.paths.as_path().join(p.as_str()).to_string_lossy().into();
              path_vars.insert(key, val);
            }
          }
        }
      }
      // try to get from the config.
      if let (None, Some(path)) = (&root.group_path, ws.root) {
        let path = root.paths.as_path().join(path.as_str());
        match GroupPath::new(fs, path.clone()) {
          Some(path) => {
            root_group_source.path = Some(config_path);
            root.group_path = Some(path);
          }
          None => {
            return Err(GetInputError {
              source: Source { path: Some(config_path), range: None },
              path,
              kind: GetInputErrorKind::NotGroup,
            })
          }
        }
      }
    }
  }
  // if not, try to get one from the root dir.
  if root.group_path.is_none() {
    let dir_entries = fs.read_dir(root.paths.as_path()).map_err(|e| GetInputError {
      source: Source::default(),
      path: root.paths.as_path().to_owned(),
      kind: GetInputErrorKind::Io(e),
    })?;
    for entry in dir_entries {
      if let Some(group_path) = GroupPath::new(fs, entry.clone()) {
        match &root.group_path {
          Some(rgp) => {
            return Err(GetInputError {
              kind: GetInputErrorKind::MultipleRoots(rgp.path.clone(), entry.clone()),
              source: Source { path: Some(rgp.path.clone()), range: None },
              path: entry,
            })
          }
          None => root.group_path = Some(group_path),
        }
      }
    }
  }
  let root_group_path = root.group_path.as_ref().ok_or_else(|| GetInputError {
    source: Source::default(),
    path: root.paths.as_path().to_owned(),
    kind: GetInputErrorKind::NoRoot,
  })?;
  Ok(RootGroup {
    path: get_path_id(fs, &mut root.paths, root_group_source, root_group_path.path.as_path())?,
    kind: root_group_path.kind,
    path_vars,
  })
}

#[derive(Debug, Clone, Copy)]
struct GroupToProcess {
  /// the path that led us to `group_path`.
  containing_path: PathId,
  /// the range in the file at `containing_path` that led us to `group_path`, if any.
  containing_range: Option<Range>,
  /// the path to process.
  group_path: PathId,
}

fn start_group_file<F>(
  root: &mut paths::Root,
  cur: GroupToProcess,
  fs: &F,
) -> Result<(paths::CanonicalPathBuf, String, text_pos::PositionDb)>
where
  F: paths::FileSystem,
{
  let group_path = root.get_path(cur.group_path).clone();
  let containing_path = root.get_path(cur.containing_path).as_path().to_owned();
  let source = Source { path: Some(containing_path), range: cur.containing_range };
  let contents = read_file(fs, source, group_path.as_path())?;
  let pos_db = text_pos::PositionDb::new(&contents);
  Ok((group_path, contents, pos_db))
}

/// The root, in which everything is contained.
#[derive(Debug)]
pub struct Root {
  paths: paths::Root,
  group_path: Option<GroupPath>,
}

impl Root {
  /// Returns this as a paths root.
  pub fn as_paths(&self) -> &paths::Root {
    &self.paths
  }

  /// Returns this as a mutable paths root.
  pub fn as_mut_paths(&mut self) -> &mut paths::Root {
    &mut self.paths
  }
}

/// Get a `Root` from a canonical root dir.
pub fn get_root_dir(path: paths::CanonicalPathBuf) -> Root {
  Root { paths: paths::Root::new(path), group_path: None }
}

/// Given a path to either a group path or a directory, return the root for it.
pub fn get_root<F>(fs: &F, path: &Path) -> Result<Root>
where
  F: paths::FileSystem,
{
  let path = canonicalize(fs, path, &Source::default())?;
  let (root_path, group_path) = match GroupPath::new(fs, path.clone().into_path_buf()) {
    None => (path, None),
    Some(path) => {
      let parent = path.as_path().parent().expect("no parent");
      let rp = fs.canonicalize(parent).expect("canonicalize parent of canonical path");
      (rp, Some(path))
    }
  };
  Ok(Root { paths: paths::Root::new(root_path), group_path })
}

/// Get some input from the filesystem.
pub fn get<F>(fs: &F, root: &mut Root) -> Result<Input>
where
  F: paths::FileSystem,
{
  let root_group = get_root_group(fs, root)?;
  let init = GroupToProcess {
    containing_path: root_group.path,
    containing_range: None,
    group_path: root_group.path,
  };
  let mut sources = PathMap::<String>::default();
  let groups = match root_group.kind {
    GroupPathKind::Cm => {
      let mut cm_files = PathMap::<CmFile>::default();
      get_cm_file(&mut root.paths, fs, &root_group.path_vars, &mut sources, &mut cm_files, init)?;
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
        if groups.contains_key(&cur.group_path) {
          continue;
        }
        let (group_path, contents, pos_db) = start_group_file(&mut root.paths, cur, fs)?;
        let group_path = group_path.as_path();
        let group_parent = group_path.parent().expect("path from get_path has no parent");
        let syntax_dec =
          mlb_syntax::get(&contents, &root_group.path_vars).map_err(|e| GetInputError {
            source: Source { path: None, range: pos_db.range(e.text_range()) },
            path: group_path.to_owned(),
            kind: GetInputErrorKind::Mlb(e),
          })?;
        let mut cx = MlbCx {
          path: group_path,
          parent: group_parent,
          pos_db: &pos_db,
          fs,
          root: &mut root.paths,
          sources: &mut sources,
          stack: &mut stack,
          path_id: cur.group_path,
        };
        let bas_dec = get_bas_dec(&mut cx, syntax_dec)?;
        groups.insert(cur.group_path, Group { bas_dec, pos_db });
      }
      groups
    }
  };
  let graph: topo_sort::Graph<_> = groups
    .iter()
    .map(|(&path, group)| {
      let mut ac = BTreeSet::<PathId>::new();
      bas_dec_paths(&mut ac, &group.bas_dec);
      (path, ac)
    })
    .collect();
  if let Err(err) = topo_sort::get(&graph) {
    return Err(GetInputError {
      source: Source::default(),
      path: root.paths.get_path(err.witness()).as_path().to_owned(),
      kind: GetInputErrorKind::Cycle,
    });
  }
  Ok(Input { sources, groups, root_group_id: root_group.path })
}

/// only derives default because we need to mark in-progress files as visited to prevent infinite
/// recursing.
#[derive(Debug, Default)]
struct CmFile {
  /// only optional so this can derive default.
  pos_db: Option<text_pos::PositionDb>,
  paths: Vec<mlb_hir::BasDec>,
  exports: Vec<Export>,
}

#[derive(Debug, Clone)]
struct Export {
  namespace: mlb_hir::Namespace,
  name: text_size_util::WithRange<sml_hir::Name>,
}

/// only recursive to support library exports, which ~necessitates the ability to know the exports
/// of a given library path on demand.
fn get_cm_file<F>(
  root: &mut paths::Root,
  fs: &F,
  path_vars: &paths::slash_var_path::Env,
  sources: &mut paths::PathMap<String>,
  cm_files: &mut paths::PathMap<CmFile>,
  cur: GroupToProcess,
) -> Result<()>
where
  F: paths::FileSystem,
{
  if cm_files.contains_key(&cur.group_path) {
    return Ok(());
  }
  // HACK: fake it so we don't infinitely recurse. this will be overwritten later.
  cm_files.insert(cur.group_path, CmFile::default());
  let (group_path, contents, pos_db) = start_group_file(root, cur, fs)?;
  let group_path = group_path.as_path();
  let group_parent = group_path.parent().expect("path from get_path has no parent");
  let cm = cm::get(&contents, path_vars).map_err(|e| GetInputError {
    source: Source { path: None, range: pos_db.range(e.text_range()) },
    path: group_path.to_owned(),
    kind: GetInputErrorKind::Cm(e),
  })?;
  let paths = cm
    .paths
    .into_iter()
    .map(|parsed_path| {
      let source =
        Source { path: Some(group_path.to_owned()), range: pos_db.range(parsed_path.range) };
      let path = group_parent.join(parsed_path.val.as_path());
      let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
      let kind = match parsed_path.val.kind() {
        cm::PathKind::Sml => {
          let contents = read_file(fs, source, path.as_path())?;
          sources.insert(path_id, contents);
          mlb_hir::PathKind::Sml
        }
        cm::PathKind::Cm => {
          let cur = GroupToProcess {
            containing_path: cur.group_path,
            containing_range: source.range,
            group_path: path_id,
          };
          get_cm_file(root, fs, path_vars, sources, cm_files, cur)?;
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
            return Err(GetInputError {
              source: Source { path: None, range: pos_db.range(ns.range) },
              path: group_path.to_owned(),
              kind: GetInputErrorKind::UnsupportedExport,
            })
          }
        };
        exports.push(Export { namespace, name });
      }
      cm::Export::Library(lib) => {
        let source = Source { path: Some(group_path.to_owned()), range: pos_db.range(lib.range) };
        let path = group_parent.join(lib.val.as_path());
        let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
        let cur = GroupToProcess {
          containing_path: cur.group_path,
          containing_range: source.range,
          group_path: path_id,
        };
        get_cm_file(root, fs, path_vars, sources, cm_files, cur)?;
        let cm_file =
          cm_files.get(&cur.group_path).expect("cm file should be set after get_cm_file");
        exports.extend(
          cm_file
            .exports
            .iter()
            .map(|ex| Export { namespace: ex.namespace, name: lib.wrap(ex.name.val.clone()) }),
        );
      }
      cm::Export::Source(range) => {
        return Err(GetInputError {
          source: Source { path: None, range: pos_db.range(range) },
          path: group_path.to_owned(),
          kind: GetInputErrorKind::UnsupportedExport,
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
    pos_db: Some(pos_db),
    paths: paths.into_iter().map(|(p, k)| mlb_hir::BasDec::Path(p, k)).collect(),
    exports,
  };
  cm_files.insert(cur.group_path, cm_file);
  Ok(())
}

fn get_path_id<F>(
  fs: &F,
  root: &mut paths::Root,
  source: Source,
  path: &Path,
) -> Result<paths::PathId>
where
  F: paths::FileSystem,
{
  let canonical = canonicalize(fs, path, &source)?;
  root.get_id(&canonical).map_err(|e| GetInputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::NotInRoot(e),
  })
}

fn canonicalize<F>(fs: &F, path: &Path, source: &Source) -> Result<paths::CanonicalPathBuf>
where
  F: paths::FileSystem,
{
  fs.canonicalize(path).map_err(|e| GetInputError {
    source: source.clone(),
    path: path.to_owned(),
    kind: GetInputErrorKind::Io(e),
  })
}

fn read_file<F>(fs: &F, source: Source, path: &Path) -> Result<String>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path).map_err(|e| GetInputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::Io(e),
  })
}

struct MlbCx<'a, F> {
  path: &'a Path,
  parent: &'a Path,
  pos_db: &'a text_pos::PositionDb,
  fs: &'a F,
  root: &'a mut paths::Root,
  sources: &'a mut PathMap<String>,
  stack: &'a mut Vec<GroupToProcess>,
  path_id: PathId,
}

fn get_bas_dec<F>(cx: &mut MlbCx<'_, F>, dec: mlb_syntax::BasDec) -> Result<mlb_hir::BasDec>
where
  F: paths::FileSystem,
{
  let ret = match dec {
    mlb_syntax::BasDec::Basis(binds) => {
      let mut names = FxHashSet::<sml_hir::Name>::default();
      let binds = binds
        .into_iter()
        .map(|(name, exp)| {
          if !names.insert(name.val.clone()) {
            return Err(GetInputError {
              source: Source { path: None, range: cx.pos_db.range(name.range) },
              path: cx.path.to_owned(),
              kind: GetInputErrorKind::Duplicate(name.val),
            });
          }
          let exp = get_bas_exp(cx, exp)?;
          Ok(mlb_hir::BasDec::Basis(name, exp.into()))
        })
        .collect::<Result<Vec<_>>>()?;
      mlb_hir::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Open(names) => {
      mlb_hir::BasDec::seq(names.into_iter().map(mlb_hir::BasDec::Open).collect())
    }
    mlb_syntax::BasDec::Local(local_dec, in_dec) => {
      mlb_hir::BasDec::Local(get_bas_dec(cx, *local_dec)?.into(), get_bas_dec(cx, *in_dec)?.into())
    }
    mlb_syntax::BasDec::Export(ns, binds) => {
      let mut names = FxHashSet::<sml_hir::Name>::default();
      let binds = binds
        .into_iter()
        .map(|(lhs, rhs)| {
          if !names.insert(lhs.val.clone()) {
            return Err(GetInputError {
              source: Source { path: None, range: cx.pos_db.range(lhs.range) },
              path: cx.path.to_owned(),
              kind: GetInputErrorKind::Duplicate(lhs.val),
            });
          }
          let rhs = rhs.unwrap_or_else(|| lhs.clone());
          let ns = match ns {
            mlb_syntax::Namespace::Structure => mlb_hir::Namespace::Structure,
            mlb_syntax::Namespace::Signature => mlb_hir::Namespace::Signature,
            mlb_syntax::Namespace::Functor => mlb_hir::Namespace::Functor,
          };
          Ok(mlb_hir::BasDec::Export(ns, lhs, rhs))
        })
        .collect::<Result<Vec<_>>>()?;
      mlb_hir::BasDec::seq(binds)
    }
    mlb_syntax::BasDec::Path(parsed_path) => {
      let source =
        Source { path: Some(cx.path.to_owned()), range: cx.pos_db.range(parsed_path.range) };
      let path = cx.parent.join(parsed_path.val.as_path());
      let path_id = get_path_id(cx.fs, cx.root, source.clone(), path.as_path())?;
      let kind = match parsed_path.val.kind() {
        mlb_syntax::PathKind::Sml => {
          let contents = read_file(cx.fs, source, path.as_path())?;
          cx.sources.insert(path_id, contents);
          mlb_hir::PathKind::Sml
        }
        mlb_syntax::PathKind::Mlb => {
          cx.stack.push(GroupToProcess {
            containing_path: cx.path_id,
            containing_range: source.range,
            group_path: path_id,
          });
          mlb_hir::PathKind::Mlb
        }
      };
      mlb_hir::BasDec::Path(path_id, kind)
    }
    mlb_syntax::BasDec::Ann(_, dec) => get_bas_dec(cx, *dec)?,
    mlb_syntax::BasDec::Seq(decs) => mlb_hir::BasDec::seq(
      decs.into_iter().map(|dec| get_bas_dec(cx, dec)).collect::<Result<Vec<_>>>()?,
    ),
  };
  Ok(ret)
}

fn get_bas_exp<F>(cx: &mut MlbCx<'_, F>, exp: mlb_syntax::BasExp) -> Result<mlb_hir::BasExp>
where
  F: paths::FileSystem,
{
  let ret = match exp {
    mlb_syntax::BasExp::Bas(dec) => mlb_hir::BasExp::Bas(get_bas_dec(cx, dec)?),
    mlb_syntax::BasExp::Name(name) => mlb_hir::BasExp::Name(name),
    mlb_syntax::BasExp::Let(dec, exp) => {
      mlb_hir::BasExp::Let(get_bas_dec(cx, dec)?, get_bas_exp(cx, *exp)?.into())
    }
  };
  Ok(ret)
}

fn bas_dec_paths(ac: &mut BTreeSet<PathId>, dec: &mlb_hir::BasDec) {
  match dec {
    mlb_hir::BasDec::Open(_) | mlb_hir::BasDec::Export(_, _, _) => {}
    mlb_hir::BasDec::Path(p, _) => {
      ac.insert(*p);
    }
    mlb_hir::BasDec::Basis(_, exp) => bas_exp_paths(ac, exp),
    mlb_hir::BasDec::Local(local_dec, in_dec) => {
      bas_dec_paths(ac, local_dec);
      bas_dec_paths(ac, in_dec);
    }
    mlb_hir::BasDec::Seq(decs) => {
      for dec in decs {
        bas_dec_paths(ac, dec);
      }
    }
  }
}

fn bas_exp_paths(ac: &mut BTreeSet<PathId>, exp: &mlb_hir::BasExp) {
  match exp {
    mlb_hir::BasExp::Bas(dec) => bas_dec_paths(ac, dec),
    mlb_hir::BasExp::Name(_) => {}
    mlb_hir::BasExp::Let(dec, exp) => {
      bas_dec_paths(ac, dec);
      bas_exp_paths(ac, exp);
    }
  }
}
