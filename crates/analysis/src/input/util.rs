//! Pervasive utilities.

use paths::PathId;
use std::fmt;
use std::path::{Path, PathBuf};
use text_pos::Range;

/// An error when getting input.
///
/// We might like this to be non-pub and just a variant of [`crate::error::Error`]. The problem is
/// that _sometimes_, a `GetInputError` is for a file which doesn't exist (or a non-file), so we
/// can't always show these in the editor inline.
#[derive(Debug)]
pub struct InputError {
  pub(crate) source: ErrorSource,
  pub(crate) path: PathBuf,
  pub(crate) kind: GetInputErrorKind,
}

impl InputError {
  /// Returns a path associated with this error, which may or may not exist.
  pub fn path(&self) -> &Path {
    self.source.path.as_ref().unwrap_or(&self.path).as_path()
  }

  /// Returns a range for this error in `path`.
  pub fn range(&self) -> Option<Range> {
    self.source.range
  }

  /// Return this as an I/O error.
  pub fn from_io(path: PathBuf, e: std::io::Error) -> Self {
    Self { source: ErrorSource::default(), path, kind: GetInputErrorKind::Io(e) }
  }

  /// Returns the error code for this.
  pub fn code(&self) -> u16 {
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
      GetInputErrorKind::HasMembersButAlsoRootOrPathVars => 1012,
      GetInputErrorKind::UnsupportedExport => 1999,
    }
  }
}

impl fmt::Display for InputError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.source.path.is_some() {
      write!(f, "{}: ", self.path.display())?;
    }
    self.kind.fmt(f)
  }
}

#[derive(Debug)]
pub(crate) enum GetInputErrorKind {
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
  Duplicate(str_util::Name),
  HasMembersButAlsoRootOrPathVars,
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
      GetInputErrorKind::HasMembersButAlsoRootOrPathVars => f.write_str(
        "cannot set `workspace.members` but also set `workspace.root` or `workspace.path-vars`",
      ),
      GetInputErrorKind::UnsupportedExport => f.write_str("unsupported export kind"),
    }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct ErrorSource {
  pub(crate) path: Option<PathBuf>,
  pub(crate) range: Option<Range>,
}

/// std's Result with GetInputError as the default error.
pub(crate) type Result<T, E = InputError> = std::result::Result<T, E>;

pub(crate) fn get_path_id<F>(
  fs: &F,
  root: &mut paths::Root,
  source: ErrorSource,
  path: &Path,
) -> Result<paths::PathId>
where
  F: paths::FileSystem,
{
  let canonical = canonicalize(fs, path, &source)?;
  root.get_id(&canonical).map_err(|e| InputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::NotInRoot(e),
  })
}

pub(crate) fn canonicalize<F>(
  fs: &F,
  path: &Path,
  source: &ErrorSource,
) -> Result<paths::CanonicalPathBuf>
where
  F: paths::FileSystem,
{
  fs.canonicalize(path).map_err(|e| InputError {
    source: source.clone(),
    path: path.to_owned(),
    kind: GetInputErrorKind::Io(e),
  })
}

pub(crate) fn read_file<F>(fs: &F, source: ErrorSource, path: &Path) -> Result<String>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path).map_err(|e| InputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::Io(e),
  })
}

pub(crate) fn read_dir<F>(fs: &F, source: ErrorSource, path: &Path) -> Result<Vec<PathBuf>>
where
  F: paths::FileSystem,
{
  fs.read_dir(path).map_err(|e| InputError {
    source,
    path: path.to_owned(),
    kind: GetInputErrorKind::Io(e),
  })
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct GroupPathToProcess {
  /// the path that led us to `path`.
  pub(crate) parent: PathId,
  /// the range in the file at `parent` that led us to `path`, if any.
  pub(crate) range: Option<Range>,
  /// the path to process.
  pub(crate) path: PathId,
}

pub(crate) struct StartedGroupFile {
  pub(crate) path: paths::CanonicalPathBuf,
  pub(crate) contents: String,
  pub(crate) pos_db: text_pos::PositionDb,
}

impl StartedGroupFile {
  pub(crate) fn new<F>(root: &mut paths::Root, cur: GroupPathToProcess, fs: &F) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let path = root.get_path(cur.path).clone();
    let containing_path = root.get_path(cur.parent).as_path().to_owned();
    let source = ErrorSource { path: Some(containing_path), range: cur.range };
    let contents = read_file(fs, source, path.as_path())?;
    let pos_db = text_pos::PositionDb::new(&contents);
    Ok(Self { path, contents, pos_db })
  }
}

/// A kind of group path.
#[derive(Debug, Clone, Copy)]
pub(crate) enum GroupPathKind {
  /// SML/NJ Compilation Manager files.
  Cm,
  /// ML Basis files.
  Mlb,
}
