use std::fmt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use str_util::Name;
use text_size_util::{TextRange, WithRange};

/// std's Result with our Error.
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) enum ErrorKind {
  UnclosedComment,
  EmptyExportList,
  Expected(Token<'static>),
  ExpectedString,
  ExpectedDesc,
  ExpectedPathOrMinus,
  ExpectedExport,
  UnsupportedClass(PathBuf, String),
  CouldNotDetermineClass(PathBuf),
  SlashVarPathError(paths::slash_var_path::Error),
}

/// An error when processing a CM file.
#[derive(Debug)]
pub struct Error(WithRange<ErrorKind>);

impl Error {
  /// Returns a text range for this error.
  #[must_use]
  pub fn text_range(&self) -> TextRange {
    self.0.range
  }

  pub(crate) fn new(kind: ErrorKind, range: TextRange) -> Self {
    Self(WithRange { val: kind, range })
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.0.val {
      ErrorKind::UnclosedComment => f.write_str("unclosed block comment"),
      ErrorKind::EmptyExportList => f.write_str("invalid empty export list"),
      ErrorKind::Expected(tok) => write!(f, "expected `{tok}`"),
      ErrorKind::ExpectedString => f.write_str("expected a string"),
      ErrorKind::ExpectedDesc => f.write_str("expected `Group`, `Library`, or `Alias`"),
      ErrorKind::ExpectedPathOrMinus => f.write_str("expected a regular path or `-`"),
      ErrorKind::ExpectedExport => f.write_str("expected an export"),
      ErrorKind::UnsupportedClass(p, c) => write!(f, "{}: unsupported class: {c}", p.display()),
      ErrorKind::CouldNotDetermineClass(p) => {
        write!(f, "{}: couldn't determine class", p.display())
      }
      ErrorKind::SlashVarPathError(e) => write!(f, "cannot construct path: {e}"),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Token<'a> {
  Structure,
  Signature,
  Functor,
  FunSig,
  Group,
  Library,
  Source,
  Is,
  Star,
  Minus,
  Colon,
  LRound,
  RRound,
  String(&'a str),
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Token::Structure => "structure",
      Token::Signature => "signature",
      Token::Functor => "functor",
      Token::FunSig => "funsig",
      Token::Group => "group",
      Token::Library => "library",
      Token::Source => "source",
      Token::Is => "is",
      Token::Star => "*",
      Token::Minus => "-",
      Token::Colon => ":",
      Token::LRound => "(",
      Token::RRound => ")",
      Token::String(s) => s,
    };
    f.write_str(s)
  }
}

/// A processed CM file.
#[derive(Debug)]
pub struct CmFile {
  /// The export.
  pub export: Export,
  /// The path.
  pub paths: Vec<WithRange<ParsedPath>>,
}

/// A kind of path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum PathKind {
  Sml,
  Cm,
}

/// A parsed path.
#[derive(Debug)]
pub struct ParsedPath {
  pub(crate) kind: PathKind,
  pub(crate) path: PathBuf,
}

impl ParsedPath {
  /// Returns the kind of path this is.
  pub fn kind(&self) -> PathKind {
    self.kind
  }

  /// Returns this as a `Path`.
  pub fn as_path(&self) -> &Path {
    self.path.as_path()
  }
}

pub(crate) struct ParseRoot {
  pub(crate) export: Export,
  pub(crate) members: Vec<Member>,
}

/// An export.
#[derive(Debug)]
pub enum Export {
  /// A named export, like `structure S`.
  Name(WithRange<Namespace>, WithRange<Name>),
  /// A re-export of another CM library.
  Library(WithRange<PathOrStdBasis>),
  /// A source export.
  Source(WithRange<PathOrMinus>),
  /// A group export.
  Group(WithRange<PathOrMinus>),
  /// A union of exports.
  Union(Vec<Export>),
  /// A difference of exports.
  Difference(Box<Export>, Box<Export>),
  /// An intersection of exports.
  Intersection(Box<Export>, Box<Export>),
}

/// Either a regular path or a std basis path (e.g. `$/basis.cm`).
#[derive(Debug, PartialEq, Eq)]
pub enum PathOrStdBasis {
  /// A path.
  Path(PathBuf),
  /// A std basis path.
  StdBasis,
}

/// The "argument" to a source or group export.
#[derive(Debug, PartialEq, Eq)]
pub enum PathOrMinus {
  /// A path.
  Path(PathBuf),
  /// A minus.
  Minus,
}

/// A namespace, like `structure` in `structure S`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Namespace {
  Structure,
  Signature,
  Functor,
  FunSig,
}

pub(crate) struct Member {
  pub(crate) pathname: WithRange<PathOrStdBasis>,
  pub(crate) class: Option<WithRange<Class>>,
}

impl Member {
  pub(crate) fn class(&self) -> Option<WithRange<Class>> {
    self.class.clone().or_else(|| match &self.pathname.val {
      PathOrStdBasis::Path(p) => Class::from_path(p.as_path()).map(|x| self.pathname.wrap(x)),
      PathOrStdBasis::StdBasis => None,
    })
  }
}

/// A class of file that may appear in a CM file.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Class {
  Sml,
  Cm,
  Other(String),
}

impl Class {
  fn from_path(path: &Path) -> Option<Self> {
    let ret = match path.extension()?.to_str()? {
      "sig" | "sml" | "fun" => Self::Sml,
      "cm" => Self::Cm,
      _ => return None,
    };
    Some(ret)
  }
}

impl FromStr for Class {
  type Err = std::convert::Infallible;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s.to_ascii_lowercase().as_str() {
      "sml" => Self::Sml,
      "cm" | "cmfile" => Self::Cm,
      s => Self::Other(s.to_owned()),
    };
    Ok(ret)
  }
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Class::Sml => f.write_str("sml"),
      Class::Cm => f.write_str("cm"),
      Class::Other(s) => f.write_str(s),
    }
  }
}
