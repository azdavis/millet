//! Input to analysis.

use paths::{PathId, PathMap};
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
  pub fn iter_sources(&self) -> impl Iterator<Item = (paths::PathId, &str)> + '_ {
    self.sources.iter().map(|(&path, s)| (path, s.as_str()))
  }
}

/// An error when getting input.
#[derive(Debug)]
pub struct GetInputError {
  source: Option<PathBuf>,
  path: PathBuf,
  kind: GetInputErrorKind,
  range: Option<Range>,
}

impl GetInputError {
  /// Returns a path associated with this error, which may or may not exist.
  pub fn path(&self) -> &Path {
    self.source.as_ref().unwrap_or(&self.path).as_path()
  }

  /// Returns a range for this error in `path`.
  pub fn range(&self) -> Option<Range> {
    self.range
  }

  /// Returns a value that displays the error message without the path.
  pub fn message(&self) -> impl fmt::Display + '_ {
    &self.kind
  }
}

impl fmt::Display for GetInputError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.path.display(), self.kind)
  }
}

impl std::error::Error for GetInputError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match &self.kind {
      GetInputErrorKind::ReadDir(e)
      | GetInputErrorKind::ReadFile(e)
      | GetInputErrorKind::Canonicalize(e) => Some(e),
      GetInputErrorKind::Cm(e) => Some(e),
      GetInputErrorKind::Mlb(e) => Some(e),
      GetInputErrorKind::NotInRoot(e) => Some(e),
      GetInputErrorKind::CouldNotParseConfig(e) => Some(e),
      GetInputErrorKind::NoParent
      | GetInputErrorKind::MultipleRootGroups(_, _)
      | GetInputErrorKind::NoRootGroup
      | GetInputErrorKind::InvalidConfigVersion(_)
      | GetInputErrorKind::Cycle
      | GetInputErrorKind::UnsupportedExport
      | GetInputErrorKind::NotGroup => None,
    }
  }
}

#[derive(Debug)]
enum GetInputErrorKind {
  ReadDir(std::io::Error),
  ReadFile(std::io::Error),
  Cm(cm::Error),
  Mlb(ml_basis::Error),
  Canonicalize(std::io::Error),
  NoParent,
  NotInRoot(std::path::StripPrefixError),
  MultipleRootGroups(PathBuf, PathBuf),
  NoRootGroup,
  CouldNotParseConfig(toml::de::Error),
  InvalidConfigVersion(u16),
  Cycle,
  UnsupportedExport,
  NotGroup,
}

impl fmt::Display for GetInputErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      GetInputErrorKind::ReadDir(e) => write!(f, "couldn't read directory: {e}"),
      GetInputErrorKind::ReadFile(e) => write!(f, "couldn't read file: {e}"),
      GetInputErrorKind::Cm(e) => write!(f, "couldn't process SML/NJ CM file: {e}"),
      GetInputErrorKind::Mlb(e) => write!(f, "couldn't process ML Basis file: {e}"),
      GetInputErrorKind::Canonicalize(e) => write!(f, "couldn't canonicalize: {e}"),
      GetInputErrorKind::NoParent => f.write_str("no parent"),
      GetInputErrorKind::NotInRoot(e) => write!(f, "not in root: {e}"),
      GetInputErrorKind::MultipleRootGroups(a, b) => write!(
        f,
        "multiple root groups: {} and {}, disambiguate with config file",
        a.display(),
        b.display()
      ),
      GetInputErrorKind::NoRootGroup => f.write_str("no root group"),
      GetInputErrorKind::CouldNotParseConfig(e) => write!(f, "couldn't parse config: {e}"),
      GetInputErrorKind::InvalidConfigVersion(n) => {
        write!(f, "invalid config version: expected 1, found {n}")
      }
      GetInputErrorKind::Cycle => f.write_str("there is a cycle involving this path"),
      GetInputErrorKind::UnsupportedExport => f.write_str("unsupported export kind"),
      GetInputErrorKind::NotGroup => f.write_str("not a group path"),
    }
  }
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
pub struct GroupPath {
  kind: GroupPathKind,
  path: PathBuf,
}

impl GroupPath {
  /// Returns a new `GroupPath`.
  pub fn new<F>(fs: &F, path: PathBuf) -> Option<GroupPath>
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
  pub fn as_path(&self) -> &Path {
    self.path.as_path()
  }
}

/// Get some input from the filesystem. If `root_group_path` is provided, it should be in the
/// `root`.
pub fn get<F>(
  fs: &F,
  root: &mut paths::Root,
  mut root_group_path: Option<GroupPath>,
) -> Result<Input>
where
  F: paths::FileSystem,
{
  let mut root_group_source = None::<PathBuf>;
  let config_path = root.as_path().join(config::FILE_NAME);
  if let Ok(contents) = fs.read_to_string(&config_path) {
    let config: config::Root = match toml::from_str(&contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(GetInputError {
          source: None,
          path: config_path,
          kind: GetInputErrorKind::CouldNotParseConfig(e),
          range: None,
        })
      }
    };
    if config.version != 1 {
      return Err(GetInputError {
        source: None,
        path: config_path,
        kind: GetInputErrorKind::InvalidConfigVersion(config.version),
        range: None,
      });
    }
    if let Some(path) = config.workspace.and_then(|workspace| workspace.root) {
      let path = root.as_path().join(path);
      match GroupPath::new(fs, path.clone()) {
        Some(path) => {
          root_group_source = Some(config_path);
          root_group_path = Some(path);
        }
        None => {
          return Err(GetInputError {
            source: Some(config_path),
            range: None,
            path,
            kind: GetInputErrorKind::NotGroup,
          })
        }
      }
    }
  }
  if root_group_path.is_none() {
    let dir_entries = fs.read_dir(root.as_path()).map_err(|e| GetInputError {
      source: None,
      range: None,
      path: root.as_path().to_owned(),
      kind: GetInputErrorKind::ReadDir(e),
    })?;
    for entry in dir_entries {
      if let Some(group_path) = GroupPath::new(fs, entry.clone()) {
        match root_group_path {
          Some(rgp) => {
            return Err(GetInputError {
              kind: GetInputErrorKind::MultipleRootGroups(rgp.path.clone(), entry.clone()),
              source: Some(rgp.path),
              path: entry,
              range: None,
            })
          }
          None => root_group_path = Some(group_path),
        }
      }
    }
  }
  let root_group_path = root_group_path.ok_or_else(|| GetInputError {
    source: None,
    range: None,
    path: root.as_path().to_owned(),
    kind: GetInputErrorKind::NoRootGroup,
  })?;
  let root_group_id = get_path_id(
    fs,
    root,
    match &root_group_source {
      Some(p) => Source::Path(p.clone()),
      None => Source::None,
    },
    root_group_path.path.as_path(),
  )?;
  let mut sources = PathMap::<String>::default();
  let mut groups = PathMap::<Group>::default();
  let mut stack = vec![((root_group_id, None), root_group_id)];
  while let Some(((containing_path_id, containing_path_range), group_path_id)) = stack.pop() {
    if groups.contains_key(&group_path_id) {
      continue;
    }
    let group_path = root.get_path(group_path_id).clone();
    let group_path = group_path.as_path();
    let containing_path = root.get_path(containing_path_id).as_path().to_owned();
    let source = match containing_path_range {
      None => Source::Path(containing_path),
      Some(r) => Source::PathAndRange(containing_path, r),
    };
    let contents = read_file(fs, source, group_path)?;
    let pos_db = text_pos::PositionDb::new(&contents);
    let group_parent = match group_path.parent() {
      Some(x) => x.to_owned(),
      None => {
        return Err(GetInputError {
          range: None,
          source: None,
          path: group_path.to_owned(),
          kind: GetInputErrorKind::NoParent,
        })
      }
    };
    let group = match root_group_path.kind {
      GroupPathKind::Cm => {
        let cm = cm::get(&contents).map_err(|e| GetInputError {
          source: None,
          path: group_path.to_owned(),
          range: Some(pos_db.range(e.text_range())),
          kind: GetInputErrorKind::Cm(e),
        })?;
        let paths = cm
          .paths
          .into_iter()
          .map(|parsed_path| {
            let range = pos_db.range(parsed_path.range);
            let source = Source::PathAndRange(group_path.to_owned(), range);
            let path = group_parent.join(parsed_path.val.as_path());
            let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
            match parsed_path.val.kind() {
              cm::PathKind::Sml => {
                let contents = read_file(fs, source, path.as_path())?;
                sources.insert(path_id, contents);
              }
              cm::PathKind::Cm => {
                stack.push(((group_path_id, Some(range)), path_id));
              }
            }
            Ok(path_id)
          })
          .collect::<Result<Vec<_>>>()?;
        let mut exports = statics::basis::Exports::default();
        for export in cm.exports {
          match export {
            cm::Export::Regular(ns, name) => match ns.val {
              cm::Namespace::Structure => exports.structure.push(hir::Name::new(name.val.as_str())),
              cm::Namespace::Signature => exports.signature.push(hir::Name::new(name.val.as_str())),
              cm::Namespace::Functor => exports.functor.push(hir::Name::new(name.val.as_str())),
              cm::Namespace::FunSig => {
                return Err(GetInputError {
                  range: Some(pos_db.range(ns.range)),
                  source: None,
                  path: group_path.to_owned(),
                  kind: GetInputErrorKind::UnsupportedExport,
                })
              }
            },
            cm::Export::Library(lib) => {
              return Err(GetInputError {
                range: Some(pos_db.range(lib.range)),
                source: None,
                path: group_path.to_owned(),
                kind: GetInputErrorKind::UnsupportedExport,
              })
            }
          }
        }
        Group { paths, exports }
      }
      GroupPathKind::Mlb => {
        let mlb = ml_basis::get(&contents).map_err(|e| GetInputError {
          source: None,
          path: group_path.to_owned(),
          range: Some(pos_db.range(e.text_range())),
          kind: GetInputErrorKind::Mlb(e),
        })?;
        let mut paths = Vec::<&located::Located<ml_basis::ParsedPath>>::new();
        // TODO this discards most of the semantics of ML Basis files. It just gets the files in the
        // right order. It totally ignores `local`, renaming exports, etc etc. So, it basically only
        // works correctly for ML Basis files that are nothing but a list of files.
        bas_dec_paths(&mut paths, &mlb);
        let paths = paths
          .into_iter()
          .filter(|p| {
            !p.val
              .as_path()
              .as_os_str()
              .to_string_lossy()
              .starts_with('$')
          })
          .map(|parsed_path| {
            let range = pos_db.range(parsed_path.range);
            let source = Source::PathAndRange(group_path.to_owned(), range);
            let path = group_parent.join(parsed_path.val.as_path());
            let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
            match parsed_path.val.kind() {
              ml_basis::PathKind::Sml => {
                let contents = read_file(fs, source, path.as_path())?;
                sources.insert(path_id, contents);
              }
              ml_basis::PathKind::Mlb => {
                stack.push(((group_path_id, Some(range)), path_id));
              }
            }
            Ok(path_id)
          })
          .collect::<Result<Vec<_>>>()?;

        Group {
          paths,
          exports: statics::basis::Exports::default(),
        }
      }
    };
    groups.insert(group_path_id, group);
  }
  let graph: topo_sort::Graph<_> = groups
    .iter()
    .map(|(&path, group)| (path, group.paths.iter().copied().collect()))
    .collect();
  if let Err(err) = topo_sort::get(&graph) {
    return Err(GetInputError {
      range: None,
      source: None,
      path: root.get_path(err.witness()).as_path().to_owned(),
      kind: GetInputErrorKind::Cycle,
    });
  }
  Ok(Input {
    sources,
    groups,
    root_group_id,
  })
}

fn bas_dec_paths<'b>(
  paths: &mut Vec<&'b located::Located<ml_basis::ParsedPath>>,
  bas_dec: &'b ml_basis::BasDec,
) {
  match bas_dec {
    ml_basis::BasDec::Local(local_dec, in_dec) => {
      bas_dec_paths(paths, local_dec);
      bas_dec_paths(paths, in_dec);
    }
    ml_basis::BasDec::Basis(_)
    | ml_basis::BasDec::Open(_)
    | ml_basis::BasDec::Structure(_)
    | ml_basis::BasDec::Signature(_)
    | ml_basis::BasDec::Functor(_) => {}
    ml_basis::BasDec::Path(path) => paths.push(path),
    ml_basis::BasDec::Ann(_, inner) => bas_dec_paths(paths, inner),
    ml_basis::BasDec::Seq(bas_decs) => {
      for bas_dec in bas_decs {
        bas_dec_paths(paths, bas_dec);
      }
    }
  }
}

#[derive(Debug, Clone)]
enum Source {
  None,
  Path(PathBuf),
  PathAndRange(PathBuf, Range),
}

impl Source {
  fn into_parts(self) -> (Option<PathBuf>, Option<Range>) {
    match self {
      Source::None => (None, None),
      Source::Path(p) => (Some(p), None),
      Source::PathAndRange(p, r) => (Some(p), Some(r)),
    }
  }
}

/// A group of paths and their exports.
#[derive(Debug)]
pub(crate) struct Group {
  pub(crate) paths: Vec<PathId>,
  pub(crate) exports: statics::basis::Exports,
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
  let (source, range) = source.into_parts();
  let canonical = fs.canonicalize(path).map_err(|e| GetInputError {
    source: source.clone(),
    range,
    path: path.to_owned(),
    kind: GetInputErrorKind::Canonicalize(e),
  })?;
  root.get_id(&canonical).map_err(|e| GetInputError {
    source,
    range,
    path: path.to_owned(),
    kind: GetInputErrorKind::NotInRoot(e),
  })
}

fn read_file<F>(fs: &F, source: Source, path: &Path) -> Result<String>
where
  F: paths::FileSystem,
{
  let (source, range) = source.into_parts();
  fs.read_to_string(path).map_err(|e| GetInputError {
    source,
    range,
    path: path.to_owned(),
    kind: GetInputErrorKind::ReadFile(e),
  })
}
