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
  UnsupportedAlias,
  UnsupportedClass(PathBuf, Class),
  CouldNotDetermineClass(PathBuf),
  SlashVarPathError(paths::slash_var_path::Error),
  AliasWithIgnoredPathVar,
}

/// An error when processing a CM file.
#[derive(Debug)]
pub struct Error(WithRange<ErrorKind>);

impl Error {
  /// Returns a text range for this error.
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
      ErrorKind::UnsupportedAlias => f.write_str("unsupported: `Alias`"),
      ErrorKind::UnsupportedClass(p, c) => write!(f, "{}: unsupported class: {c}", p.display()),
      ErrorKind::CouldNotDetermineClass(p) => {
        write!(f, "{}: couldn't determine class", p.display())
      }
      ErrorKind::SlashVarPathError(e) => write!(f, "cannot construct path: {e}"),
      ErrorKind::AliasWithIgnoredPathVar => {
        f.write_str("cannot use `alias` with a path containing an ignored variable")
      }
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
  Structure,
  Signature,
  Functor,
  FunSig,
  Group,
  Library,
  Alias,
  Is,
  Colon,
  LRound,
  RRound,
  String(&'a str),
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Token::Structure => f.write_str("structure"),
      Token::Signature => f.write_str("signature"),
      Token::Functor => f.write_str("functor"),
      Token::FunSig => f.write_str("funsig"),
      Token::Group => f.write_str("Group"),
      Token::Library => f.write_str("Library"),
      Token::Alias => f.write_str("Alias"),
      Token::Is => f.write_str("is"),
      Token::Colon => f.write_str(":"),
      Token::LRound => f.write_str("("),
      Token::RRound => f.write_str(")"),
      Token::String(s) => s.fmt(f),
    }
  }
}

/// A processed CM file.
#[derive(Debug)]
pub struct CmFile {
  /// The exports.
  pub exports: Vec<Export>,
  /// The paths, in order.
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

pub(crate) enum Root {
  Alias(WithRange<PathBuf>),
  Desc(DescKind, Vec<Export>, Vec<Member>),
}

pub(crate) enum DescKind {
  Group,
  Library,
}

/// An export, like `structure S`.
#[derive(Debug)]
pub enum Export {
  /// A 'regular' export.
  Regular(WithRange<Namespace>, WithRange<Name>),
  /// A re-export of another CM library.
  Library(WithRange<PathBuf>),
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
  pub(crate) pathname: WithRange<PathBuf>,
  pub(crate) class: Option<WithRange<Class>>,
}

impl Member {
  pub(crate) fn class(&self) -> Option<WithRange<Class>> {
    self
      .class
      .clone()
      .or_else(|| Class::from_path(self.pathname.val.as_path()).map(|x| self.pathname.wrap(x)))
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
