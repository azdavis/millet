use fast_hash::FxHashSet;
use paths::{PathId, PathMap};
use std::fmt;
use text_pos::Range;

/// The input to analysis.
#[derive(Debug, Default)]
pub struct Input {
  /// A map from source files to their contents.
  pub(crate) sources: PathMap<String>,
  /// A map from group files to their (parsed) contents.
  pub(crate) groups: PathMap<Group>,
}

impl Input {
  /// Return an iterator over the source files.
  pub fn iter_sources(&self) -> impl Iterator<Item = (paths::PathId, &str)> + '_ {
    self.sources.iter().map(|(&path, s)| (path, s.as_str()))
  }
}

/// An error when getting input.
#[derive(Debug)]
pub struct GetInputError {
  source: Option<std::path::PathBuf>,
  path: std::path::PathBuf,
  kind: GetInputErrorKind,
  range: Option<Range>,
}

impl GetInputError {
  fn new(
    source: Option<&std::path::Path>,
    path: &std::path::Path,
    kind: GetInputErrorKind,
  ) -> Self {
    Self {
      source: source.map(ToOwned::to_owned),
      path: path.to_owned(),
      kind,
      range: None,
    }
  }

  /// Returns a path associated with this error, which may or may not exist.
  pub fn path(&self) -> &std::path::Path {
    self.source.as_ref().unwrap_or(&self.path).as_path()
  }

  /// Returns a range for this error in `path`.
  pub fn range(&self) -> Option<Range> {
    self.range
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
  MultipleRootGroups(std::path::PathBuf, std::path::PathBuf),
  NoRootGroup,
  CouldNotParseConfig(toml::de::Error),
  InvalidConfigVersion(u16),
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
        "multiple root groups: {} and {}",
        a.display(),
        b.display()
      ),
      GetInputErrorKind::NoRootGroup => f.write_str("no root group"),
      GetInputErrorKind::CouldNotParseConfig(e) => write!(f, "couldn't parse config: {e}"),
      GetInputErrorKind::InvalidConfigVersion(n) => {
        write!(f, "invalid config version: expected 1, found {n}")
      }
    }
  }
}

/// Get some input from the filesystem.
pub fn get_input<F>(fs: &F, root: &mut paths::Root) -> Result<Input, GetInputError>
where
  F: paths::FileSystem,
{
  let mut ret = Input::default();
  let mut root_group_path = None::<std::path::PathBuf>;
  let mut root_group_source = None::<std::path::PathBuf>;
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
    let dir_entries = fs
      .read_dir(root.as_path())
      .map_err(|e| GetInputError::new(None, root.as_path(), GetInputErrorKind::ReadDir(e)))?;
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
  let root_group_path = root_group_path
    .ok_or_else(|| GetInputError::new(None, root.as_path(), GetInputErrorKind::NoRootGroup))?;
  let root_group_id = get_path_id(
    fs,
    root,
    root_group_source.as_deref(),
    root_group_path.as_path(),
  )?;
  let mut stack = vec![(root_group_id, root_group_id)];
  while let Some((containing_path_id, group_path_id)) = stack.pop() {
    let group_path = root.get_path(group_path_id).clone();
    let group_path = group_path.as_path();
    let containing_path = root.get_path(containing_path_id).as_path();
    let contents = read_file(fs, Some(containing_path), group_path)?;
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
        return Err(GetInputError::new(
          None,
          group_path,
          GetInputErrorKind::NoParent,
        ))
      }
    };
    let mut source_files = Vec::<paths::PathId>::new();
    for path in cm.sml {
      let path = group_parent.join(path.val.as_path());
      let path_id = get_path_id(fs, root, Some(group_path), path.as_path())?;
      let contents = read_file(fs, Some(group_path), path.as_path())?;
      source_files.push(path_id);
      ret.sources.insert(path_id, contents);
    }
    let mut dependencies = FxHashSet::<paths::PathId>::default();
    for path in cm.cm {
      let path = group_parent.join(path.val.as_path());
      let path_id = get_path_id(fs, root, Some(group_path), path.as_path())?;
      stack.push((group_path_id, path_id));
      dependencies.insert(path_id);
    }
    let group = Group {
      source_files,
      dependencies,
    };
    ret.groups.insert(group_path_id, group);
  }
  Ok(ret)
}

/// A group of source files.
///
/// TODO use exports
#[derive(Debug)]
pub(crate) struct Group {
  /// The source file paths, in order.
  pub(crate) source_files: Vec<PathId>,
  /// The dependencies of this group on other groups.
  pub(crate) dependencies: FxHashSet<PathId>,
}

fn get_path_id<F>(
  fs: &F,
  root: &mut paths::Root,
  source: Option<&std::path::Path>,
  path: &std::path::Path,
) -> Result<paths::PathId, GetInputError>
where
  F: paths::FileSystem,
{
  let canonical = fs
    .canonicalize(path)
    .map_err(|e| GetInputError::new(source, path, GetInputErrorKind::Canonicalize(e)))?;
  root
    .get_id(&canonical)
    .map_err(|e| GetInputError::new(source, path, GetInputErrorKind::NotInRoot(e)))
}

fn read_file<F>(
  fs: &F,
  source: Option<&std::path::Path>,
  path: &std::path::Path,
) -> Result<String, GetInputError>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path)
    .map_err(|e| GetInputError::new(source, path, GetInputErrorKind::ReadFile(e)))
}
