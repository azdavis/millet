use fast_hash::FxHashSet;
use paths::{PathId, PathMap};
use std::fmt;

/// The name of the root CM file we look for.
pub const ROOT_GROUP: &str = "sources.cm";

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
  path: std::path::PathBuf,
  kind: GetInputErrorKind,
}

impl GetInputError {
  fn new(path: &std::path::Path, kind: GetInputErrorKind) -> Self {
    Self {
      path: path.to_owned(),
      kind,
    }
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
      GetInputErrorKind::ReadFile(e) => Some(e),
      GetInputErrorKind::Cm(e) => Some(e),
      GetInputErrorKind::Canonicalize(e) => Some(e),
      GetInputErrorKind::NoParent => None,
      GetInputErrorKind::NotInRoot(e) => Some(e),
    }
  }
}

#[derive(Debug)]
enum GetInputErrorKind {
  ReadFile(std::io::Error),
  Cm(cm::Error),
  Canonicalize(std::io::Error),
  NoParent,
  NotInRoot(std::path::StripPrefixError),
}

impl fmt::Display for GetInputErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      GetInputErrorKind::ReadFile(e) => write!(f, "couldn't read file: {e}"),
      GetInputErrorKind::Cm(e) => write!(f, "couldn't process CM file: {e}"),
      GetInputErrorKind::Canonicalize(e) => write!(f, "couldn't canonicalize: {e}"),
      GetInputErrorKind::NoParent => f.write_str("no parent"),
      GetInputErrorKind::NotInRoot(e) => write!(f, "not in root: {e}"),
    }
  }
}

/// Get some input from the filesystem.
pub fn get_input<F>(fs: &F, root: &mut paths::Root) -> Result<Input, GetInputError>
where
  F: paths::FileSystem,
{
  let mut ret = Input::default();
  let root_group_id = get_path_id(fs, root, root.as_path().join(ROOT_GROUP).as_path())?;
  let mut stack = vec![root_group_id];
  while let Some(path_id) = stack.pop() {
    let path = root.get_path(path_id).as_path();
    let s = read_file(fs, path)?;
    let cm = cm::get(&s).map_err(|e| GetInputError::new(path, GetInputErrorKind::Cm(e)))?;
    let parent = match path.parent() {
      Some(x) => x.to_owned(),
      None => return Err(GetInputError::new(path, GetInputErrorKind::NoParent)),
    };
    let mut source_files = Vec::<paths::PathId>::new();
    for path in cm.sml {
      let path = parent.join(path.as_path());
      let path_id = get_path_id(fs, root, path.as_path())?;
      let s = read_file(fs, path.as_path())?;
      source_files.push(path_id);
      ret.sources.insert(path_id, s);
    }
    let mut dependencies = FxHashSet::<paths::PathId>::default();
    for path in cm.cm {
      let path = parent.join(path.as_path());
      let path_id = get_path_id(fs, root, path.as_path())?;
      stack.push(path_id);
      dependencies.insert(path_id);
    }
    let group = Group {
      source_files,
      dependencies,
    };
    ret.groups.insert(path_id, group);
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
  path: &std::path::Path,
) -> Result<paths::PathId, GetInputError>
where
  F: paths::FileSystem,
{
  let canonical = fs
    .canonicalize(path)
    .map_err(|e| GetInputError::new(path, GetInputErrorKind::Canonicalize(e)))?;
  root
    .get_id(&canonical)
    .map_err(|e| GetInputError::new(path, GetInputErrorKind::NotInRoot(e)))
}

fn read_file<F>(fs: &F, path: &std::path::Path) -> Result<String, GetInputError>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path)
    .map_err(|e| GetInputError::new(path, GetInputErrorKind::ReadFile(e)))
}
