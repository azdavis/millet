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
      GetInputErrorKind::ReadDir(e) => Some(e),
      GetInputErrorKind::ReadFile(e) => Some(e),
      GetInputErrorKind::Cm(e) => Some(e),
      GetInputErrorKind::Canonicalize(e) => Some(e),
      GetInputErrorKind::NoParent => None,
      GetInputErrorKind::NotInRoot(e) => Some(e),
      GetInputErrorKind::MultipleRootGroups(_, _) => None,
      GetInputErrorKind::NoRootGroup => None,
      GetInputErrorKind::CouldNotParseConfig(e) => Some(e),
      GetInputErrorKind::InvalidConfigVersion(_) => None,
      GetInputErrorKind::Cycle => None,
    }
  }
}

#[derive(Debug)]
enum GetInputErrorKind {
  ReadDir(std::io::Error),
  ReadFile(std::io::Error),
  Cm(cm::Error),
  Canonicalize(std::io::Error),
  NoParent,
  NotInRoot(std::path::StripPrefixError),
  MultipleRootGroups(PathBuf, PathBuf),
  NoRootGroup,
  CouldNotParseConfig(toml::de::Error),
  InvalidConfigVersion(u16),
  Cycle,
}

impl fmt::Display for GetInputErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      GetInputErrorKind::ReadDir(e) => write!(f, "couldn't read directory: {e}"),
      GetInputErrorKind::ReadFile(e) => write!(f, "couldn't read file: {e}"),
      GetInputErrorKind::Cm(e) => write!(f, "couldn't process CM file: {e}"),
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
    }
  }
}

/// std's Result with GetInputError as the default error.
pub type Result<T, E = GetInputError> = std::result::Result<T, E>;

/// Get some input from the filesystem. If `root_group_path` is provided, it should be in the
/// `root`.
pub fn get_input<F>(
  fs: &F,
  root: &mut paths::Root,
  mut root_group_path: Option<PathBuf>,
) -> Result<Input>
where
  F: paths::FileSystem,
{
  let mut root_group_source = None::<PathBuf>;
  let config_file_name = root.as_path().join(config::FILE_NAME);
  if let Ok(contents) = fs.read_to_string(&config_file_name) {
    let config: config::Root = match toml::from_str(&contents) {
      Ok(x) => x,
      Err(e) => {
        return Err(GetInputError {
          source: None,
          path: config_file_name,
          kind: GetInputErrorKind::CouldNotParseConfig(e),
          range: None,
        })
      }
    };
    if config.version != 1 {
      return Err(GetInputError {
        source: None,
        path: config_file_name,
        kind: GetInputErrorKind::InvalidConfigVersion(config.version),
        range: None,
      });
    }
    if let Some(path) = config.workspace.and_then(|workspace| workspace.root) {
      root_group_source = Some(config_file_name);
      root_group_path = Some(root.as_path().join(path));
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
      if entry.extension().map_or(false, |x| x == "cm") {
        match &root_group_path {
          Some(x) => {
            return Err(GetInputError {
              kind: GetInputErrorKind::MultipleRootGroups(x.clone(), entry.clone()),
              source: root_group_path,
              path: entry,
              range: None,
            })
          }
          None => root_group_path = Some(entry),
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
    root_group_path.as_path(),
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
    let cm = cm::get(&contents).map_err(|e| GetInputError {
      source: None,
      path: group_path.to_owned(),
      range: Some(pos_db.range(e.text_range())),
      kind: GetInputErrorKind::Cm(e),
    })?;
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
    let paths = cm
      .paths
      .into_iter()
      .map(|(path, kind)| {
        let range = pos_db.range(path.range);
        let source = Source::PathAndRange(group_path.to_owned(), range);
        let path = group_parent.join(path.val.as_path());
        let path_id = get_path_id(fs, root, source.clone(), path.as_path())?;
        match kind {
          cm::FileKind::Sml => {
            let contents = read_file(fs, source, path.as_path())?;
            sources.insert(path_id, contents);
          }
          cm::FileKind::Cm => {
            stack.push(((group_path_id, Some(range)), path_id));
          }
        }
        Ok(path_id)
      })
      .collect::<Result<Vec<_>>>()?;
    groups.insert(group_path_id, Group { paths });
  }
  let graph: topo_sort::Graph<_> = groups
    .iter()
    .map(|(&path, group)| (path, group.paths.iter().copied().collect()))
    .collect();
  if let Err(e) = topo_sort::get(&graph) {
    let w = e.witness();
    return Err(GetInputError {
      range: None,
      source: None,
      path: root.get_path(w).as_path().to_owned(),
      kind: GetInputErrorKind::Cycle,
    });
  }
  Ok(Input {
    sources,
    groups,
    root_group_id,
  })
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

/// A group of paths.
#[derive(Debug)]
pub(crate) struct Group {
  pub(crate) paths: Vec<PathId>,
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
