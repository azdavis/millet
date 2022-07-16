//! Input to analysis.

use paths::{PathId, PathMap};
use std::fmt;
use std::path::{Path, PathBuf};
use text_pos::Range;

/// TODO rm/set to true
pub(crate) const STRICT_EXPORTS: bool = false;

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
  pub fn iter_sources(&self) -> impl Iterator<Item = (PathId, &str)> + '_ {
    self.sources.iter().map(|(&path, s)| (path, s.as_str()))
  }
}

/// An error when getting input.
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
  Mlb(mlb_syntax::Error),
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
        "multiple root groups: {} and {}, disambiguate with `millet.toml` config file",
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
  let mut root_group_source = Source::default();
  // try to get from the config.
  if root_group_path.is_none() {
    let config_path = root.as_path().join(config::FILE_NAME);
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
      if let Some(path) = config.workspace.and_then(|workspace| workspace.root) {
        let path = root.as_path().join(path);
        match GroupPath::new(fs, path.clone()) {
          Some(path) => {
            root_group_source.path = Some(config_path);
            root_group_path = Some(path);
          }
          None => {
            return Err(GetInputError {
              source: Source {
                path: Some(config_path),
                range: None,
              },
              path,
              kind: GetInputErrorKind::NotGroup,
            })
          }
        }
      }
    }
  }
  // if not, try to get one from the root dir.
  if root_group_path.is_none() {
    let dir_entries = fs.read_dir(root.as_path()).map_err(|e| GetInputError {
      source: Source::default(),
      path: root.as_path().to_owned(),
      kind: GetInputErrorKind::ReadDir(e),
    })?;
    for entry in dir_entries {
      if let Some(group_path) = GroupPath::new(fs, entry.clone()) {
        match root_group_path {
          Some(rgp) => {
            return Err(GetInputError {
              kind: GetInputErrorKind::MultipleRootGroups(rgp.path.clone(), entry.clone()),
              source: Source {
                path: Some(rgp.path),
                range: None,
              },
              path: entry,
            })
          }
          None => root_group_path = Some(group_path),
        }
      }
    }
  }
  let root_group_path = root_group_path.ok_or_else(|| GetInputError {
    source: Source::default(),
    path: root.as_path().to_owned(),
    kind: GetInputErrorKind::NoRootGroup,
  })?;
  let root_group_id = get_path_id(fs, root, root_group_source, root_group_path.path.as_path())?;
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
    let source = Source {
      path: Some(containing_path),
      range: containing_path_range,
    };
    let contents = read_file(fs, source, group_path)?;
    let pos_db = text_pos::PositionDb::new(&contents);
    let group_parent = match group_path.parent() {
      Some(x) => x.to_owned(),
      None => {
        return Err(GetInputError {
          source: Source::default(),
          path: group_path.to_owned(),
          kind: GetInputErrorKind::NoParent,
        })
      }
    };
    let group = match root_group_path.kind {
      GroupPathKind::Cm => {
        let cm = cm::get(&contents).map_err(|e| GetInputError {
          source: Source {
            path: None,
            range: pos_db.range(e.text_range()),
          },
          path: group_path.to_owned(),
          kind: GetInputErrorKind::Cm(e),
        })?;
        let paths = cm
          .paths
          .into_iter()
          .map(|parsed_path| {
            let range = pos_db.range(parsed_path.range);
            let source = Source {
              path: Some(group_path.to_owned()),
              range,
            };
            let path = group_parent.join(parsed_path.val.as_path());
            let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
            match parsed_path.val.kind() {
              cm::PathKind::Sml => {
                let contents = read_file(fs, source, path.as_path())?;
                sources.insert(path_id, contents);
              }
              cm::PathKind::Cm => {
                stack.push(((group_path_id, range), path_id));
              }
            }
            Ok(path_id)
          })
          .collect::<Result<Vec<_>>>()?;
        let mut exports = statics::basis::Exports::default();
        for export in cm.exports {
          match export {
            cm::Export::Regular(ns, name) => match ns.val {
              cm::Namespace::Structure => exports.structure.push(name.val.clone()),
              cm::Namespace::Signature => exports.signature.push(name.val.clone()),
              cm::Namespace::Functor => exports.functor.push(name.val.clone()),
              cm::Namespace::FunSig => {
                return Err(GetInputError {
                  source: Source {
                    path: None,
                    range: pos_db.range(ns.range),
                  },
                  path: group_path.to_owned(),
                  kind: GetInputErrorKind::UnsupportedExport,
                })
              }
            },
            cm::Export::Library(lib) => {
              if STRICT_EXPORTS {
                return Err(GetInputError {
                  source: Source {
                    path: None,
                    range: pos_db.range(lib.range),
                  },
                  path: group_path.to_owned(),
                  kind: GetInputErrorKind::UnsupportedExport,
                });
              }
            }
          }
        }
        Group { paths, exports }
      }
      GroupPathKind::Mlb => {
        let mlb_syntax = mlb_syntax::get(&contents).map_err(|e| GetInputError {
          source: Source {
            path: None,
            range: pos_db.range(e.text_range()),
          },
          path: group_path.to_owned(),
          kind: GetInputErrorKind::Mlb(e),
        })?;
        let mut paths = Vec::<&located::Located<mlb_syntax::ParsedPath>>::new();
        // TODO this discards most of the semantics of ML Basis files. It just gets the files in the
        // right order. It totally ignores `local`, renaming exports, etc etc. So, it basically only
        // works correctly for ML Basis files that are nothing but a list of files.
        bas_dec_paths(&mut paths, &mlb_syntax);
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
            let source = Source {
              path: Some(group_path.to_owned()),
              range,
            };
            let path = group_parent.join(parsed_path.val.as_path());
            let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
            match parsed_path.val.kind() {
              mlb_syntax::PathKind::Sml => {
                let contents = read_file(fs, source, path.as_path())?;
                sources.insert(path_id, contents);
              }
              mlb_syntax::PathKind::Mlb => {
                stack.push(((group_path_id, range), path_id));
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
      source: Source::default(),
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
  paths: &mut Vec<&'b located::Located<mlb_syntax::ParsedPath>>,
  bas_dec: &'b mlb_syntax::BasDec,
) {
  match bas_dec {
    mlb_syntax::BasDec::Local(local_dec, in_dec) => {
      bas_dec_paths(paths, local_dec);
      bas_dec_paths(paths, in_dec);
    }
    mlb_syntax::BasDec::Basis(_)
    | mlb_syntax::BasDec::Open(_)
    | mlb_syntax::BasDec::Export(_, _) => {}
    mlb_syntax::BasDec::Path(path) => paths.push(path),
    mlb_syntax::BasDec::Ann(_, inner) => bas_dec_paths(paths, inner),
    mlb_syntax::BasDec::Seq(bas_decs) => {
      for bas_dec in bas_decs {
        bas_dec_paths(paths, bas_dec);
      }
    }
  }
}

#[derive(Debug, Default, Clone)]
struct Source {
  path: Option<PathBuf>,
  range: Option<Range>,
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
  let canonical = fs.canonicalize(path).map_err(|e| GetInputError {
    source: source.clone(),
    path: path.to_owned(),
    kind: GetInputErrorKind::Canonicalize(e),
  })?;
  root.get_id(&canonical).map_err(|e| GetInputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::NotInRoot(e),
  })
}

fn read_file<F>(fs: &F, source: Source, path: &Path) -> Result<String>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path).map_err(|e| GetInputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::ReadFile(e),
  })
}
