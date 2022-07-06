//! TODO prefer just a (new type) string over PathBuf?

use smol_str::SmolStr;
use std::fmt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use text_size::TextRange;

/// std's Result with our Error.
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone, Copy)]
pub struct Located<T> {
  pub val: T,
  pub range: TextRange,
}

impl<T> Located<T> {
  pub(crate) fn wrap<U>(&self, val: U) -> Located<U> {
    Located {
      val,
      range: self.range,
    }
  }
}

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
}

/// An error when processing a CM file.
#[derive(Debug)]
pub struct Error(Located<ErrorKind>);

impl Error {
  /// Returns a text range for this error.
  pub fn text_range(&self) -> TextRange {
    self.0.range
  }

  pub(crate) fn new(kind: ErrorKind, range: TextRange) -> Self {
    Self(Located { val: kind, range })
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
    }
  }
}

impl std::error::Error for Error {}

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
pub struct CMFile {
  /// The exports.
  pub exports: Vec<Export>,
  /// The SML files, in order.
  pub sml: Vec<Located<std::path::PathBuf>>,
  /// The CM files, in order.
  pub cm: Vec<Located<std::path::PathBuf>>,
}

/// A name, like `S` in `structure S`.
#[derive(Debug, PartialEq, Eq)]
pub struct Name(SmolStr);

impl Name {
  pub(crate) fn new(s: &str) -> Self {
    Self(s.into())
  }

  /// Return this as a str reference.
  pub fn as_str(&self) -> &str {
    self.0.as_str()
  }
}

pub(crate) enum Root {
  Alias(Located<PathBuf>),
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
  Regular(Located<Namespace>, Located<Name>),
  /// A re-export of another CM library.
  Library(Located<PathBuf>),
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
  pub(crate) pathname: Located<PathBuf>,
  pub(crate) class: Option<Located<Class>>,
}

impl Member {
  pub(crate) fn class(&self) -> Option<Located<Class>> {
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
