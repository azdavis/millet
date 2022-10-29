//! Pervasive utilities.

use diagnostic_util::{Code, Severity};
use paths::PathId;
use std::fmt;
use std::path::{Path, PathBuf};
use text_pos::Range;

/// An error when getting input.
///
/// We might like this to be non-pub. The problem is that _sometimes_, a `GetInputError` is for a
/// file which doesn't exist (or a non-file), so we can't always show these in the editor inline.
#[derive(Debug)]
pub struct Error {
  pub(crate) source: ErrorSource,
  pub(crate) path: PathBuf,
  pub(crate) kind: ErrorKind,
}

impl Error {
  /// Returns `abs_path`, but possibly relative to the `root`.
  ///
  /// The path will be relative to `root` if it is contained in `root`. Else, it will be absolute.
  #[must_use]
  pub fn maybe_rel_path(&self, root: &Path) -> &Path {
    maybe_rel_to_root(root, self.abs_path())
  }

  /// Returns an absolute path for this error, which may or may not exist.
  #[must_use]
  pub fn abs_path(&self) -> &Path {
    self.source.path.as_ref().unwrap_or(&self.path).as_path()
  }

  /// Returns a range for this error in `path`.
  #[must_use]
  pub fn range(&self) -> Option<Range> {
    self.source.range
  }

  /// Return this as an I/O error.
  #[must_use]
  pub fn from_io(path: PathBuf, e: std::io::Error) -> Self {
    Self { source: ErrorSource::default(), path, kind: ErrorKind::Io(e) }
  }

  /// Returns the code for this.
  #[must_use]
  pub fn code(&self) -> Code {
    match self.kind {
      ErrorKind::Io(_) => Code::n(1001),
      ErrorKind::NotInRoot(_) => Code::n(1002),
      ErrorKind::MultipleRoots(_, _) => Code::n(1003),
      ErrorKind::NoRoot => Code::n(1004),
      ErrorKind::NotGroup => Code::n(1005),
      ErrorKind::CouldNotParseConfig(_) => Code::n(1006),
      ErrorKind::InvalidConfigVersion(_) => Code::n(1007),
      ErrorKind::Cm(_) => Code::n(1008),
      ErrorKind::Mlb(_) => Code::n(1009),
      ErrorKind::Cycle => Code::n(1010),
      ErrorKind::Duplicate(_) => Code::n(1011),
      ErrorKind::InvalidErrorCode(_, _) => Code::n(1012),
      ErrorKind::SourcePathNotInFiles => Code::n(1013),
      ErrorKind::HasMembersButAlsoOtherSettings => Code::n(1995),
      ErrorKind::UnsupportedExport => Code::n(1999),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    Severity::Error
  }

  /// Returns a value that displays this.
  ///
  /// Any paths in the error will be relative to `root` if they are contained in `root`.
  #[must_use]
  pub fn display<'a>(&'a self, root: &'a Path) -> impl fmt::Display + 'a {
    ErrorDisplay { err: self, root }
  }
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
  Io(std::io::Error),
  /// TODO use or rm
  #[allow(dead_code)]
  NotInRoot(std::path::StripPrefixError),
  MultipleRoots(PathBuf, PathBuf),
  NoRoot,
  NotGroup,
  CouldNotParseConfig(toml::de::Error),
  InvalidConfigVersion(u16),
  Cm(cm_syntax::Error),
  Mlb(mlb_syntax::Error),
  Cycle,
  Duplicate(str_util::Name),
  InvalidErrorCode(str_util::SmolStr, diagnostic_util::ParseCodeError),
  SourcePathNotInFiles,
  HasMembersButAlsoOtherSettings,
  /// must be last
  UnsupportedExport,
}

struct ErrorDisplay<'a> {
  err: &'a Error,
  root: &'a Path,
}

impl fmt::Display for ErrorDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.err.kind {
      ErrorKind::Io(e) => write!(f, "couldn't perform file I/O: {e}"),
      ErrorKind::NotInRoot(e) => write!(f, "path not contained in root: {e}"),
      ErrorKind::MultipleRoots(a, b) => {
        let a = maybe_rel_to_root(self.root, a);
        let b = maybe_rel_to_root(self.root, b);
        write!(f, "multiple root group files: {} and {}", a.display(), b.display())
      }
      ErrorKind::NoRoot => f.write_str("no root group file"),
      ErrorKind::NotGroup => f.write_str("not a group file path"),
      ErrorKind::CouldNotParseConfig(e) => write!(f, "couldn't parse config: {e}"),
      ErrorKind::InvalidConfigVersion(n) => {
        write!(f, "invalid config version: expected 1, found {n}")
      }
      ErrorKind::Cm(e) => write!(f, "couldn't process SML/NJ CM file: {e}"),
      ErrorKind::Mlb(e) => write!(f, "couldn't process ML Basis file: {e}"),
      ErrorKind::Cycle => f.write_str("there is a cycle involving this path"),
      ErrorKind::Duplicate(name) => write!(f, "duplicate name: {name}"),
      ErrorKind::InvalidErrorCode(ec, e) => write!(f, "invalid error code: {ec}: {e}"),
      ErrorKind::SourcePathNotInFiles => f.write_str("`source` export not in file list"),
      ErrorKind::HasMembersButAlsoOtherSettings => {
        f.write_str("cannot set `workspace.members` but also set other configuration settings")
      }
      ErrorKind::UnsupportedExport => f.write_str("unsupported export kind"),
    }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct ErrorSource {
  pub(crate) path: Option<PathBuf>,
  pub(crate) range: Option<Range>,
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

fn maybe_rel_to_root<'a, 'r>(root: &'r Path, path: &'a Path) -> &'a Path {
  path.strip_prefix(root).unwrap_or(path)
}

pub(crate) fn get_path_id<F>(
  fs: &F,
  store: &mut paths::Store,
  source: ErrorSource,
  path: &Path,
) -> Result<paths::PathId>
where
  F: paths::FileSystem,
{
  let canonical = canonicalize(fs, path, source)?;
  Ok(store.get_id(&canonical))
}

pub(crate) fn get_path_id_in_group<F>(
  fs: &F,
  store: &mut paths::Store,
  group: &StartedGroup,
  path: &Path,
  range: text_size_util::TextRange,
) -> Result<(paths::PathId, paths::CanonicalPathBuf, ErrorSource)>
where
  F: paths::FileSystem,
{
  let source =
    ErrorSource { path: Some(group.path.as_path().to_owned()), range: group.pos_db.range(range) };
  let path = group.path.as_path().parent().expect("group path with no parent").join(path);
  let canonical = canonicalize(fs, path.as_path(), source.clone())?;
  let id = store.get_id(&canonical);
  Ok((id, canonical, source))
}

pub(crate) fn canonicalize<F>(
  fs: &F,
  path: &Path,
  source: ErrorSource,
) -> Result<paths::CanonicalPathBuf>
where
  F: paths::FileSystem,
{
  fs.canonicalize(path).map_err(|e| Error { source, path: path.to_owned(), kind: ErrorKind::Io(e) })
}

pub(crate) fn read_file<F>(fs: &F, source: ErrorSource, path: &Path) -> Result<String>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path).map_err(|e| Error {
    source,
    path: path.to_owned(),
    kind: ErrorKind::Io(e),
  })
}

pub(crate) fn read_dir<F>(fs: &F, source: ErrorSource, path: &Path) -> Result<Vec<PathBuf>>
where
  F: paths::FileSystem,
{
  fs.read_dir(path).map_err(|e| Error { source, path: path.to_owned(), kind: ErrorKind::Io(e) })
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

pub(crate) struct StartedGroup {
  pub(crate) path: paths::CanonicalPathBuf,
  pub(crate) contents: String,
  pub(crate) pos_db: text_pos::PositionDb,
}

impl StartedGroup {
  pub(crate) fn new<F>(store: &mut paths::Store, cur: GroupPathToProcess, fs: &F) -> Result<Self>
  where
    F: paths::FileSystem,
  {
    let path = store.get_path(cur.path).clone();
    let containing_path = store.get_path(cur.parent).as_path().to_owned();
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
