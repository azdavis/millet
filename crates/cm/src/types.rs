//! TODO prefer just a (new type) string over PathBuf?

use smol_str::SmolStr;
use std::fmt;
use std::path::{Path, PathBuf};
use std::str::FromStr;

/// std's Result with our Error.
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// An error when processing a CM file.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Error {
  UnclosedComment,
  EmptyExportList,
  Expected(Token<'static>),
  ExpectedString,
  ExpectedClass,
  ExpectedDesc,
  UnsupportedAlias,
  UnsupportedClass(PathBuf, Class),
  UnknownClass(PathBuf),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::UnclosedComment => f.write_str("unclosed block comment"),
      Error::EmptyExportList => f.write_str("invalid empty export list"),
      Error::Expected(tok) => write!(f, "expected `{tok}`"),
      Error::ExpectedString => f.write_str("expected a string"),
      Error::ExpectedClass => f.write_str("expected a class"),
      Error::ExpectedDesc => f.write_str("expected `Group`, `Library`, or `Alias`"),
      Error::UnsupportedAlias => f.write_str("unsupported: `Alias`"),
      Error::UnsupportedClass(p, c) => write!(f, "{}: unsupported class: {c}", p.display()),
      Error::UnknownClass(p) => write!(f, "{}: couldn't determine class", p.display()),
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
  LibraryUpper,
  LibraryLower,
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
      Token::LibraryUpper => f.write_str("Library"),
      Token::LibraryLower => f.write_str("library"),
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
  pub sml: Vec<std::path::PathBuf>,
  /// The CM files, in order.
  pub cm: Vec<std::path::PathBuf>,
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
  Alias(PathBuf),
  Desc(DescKind, Vec<Export>, Vec<Member>),
}

pub(crate) enum DescKind {
  Group,
  Library,
}

/// An export, like `structure S`.
#[derive(Debug, PartialEq, Eq)]
pub enum Export {
  /// A 'regular' export.
  Regular(Namespace, Name),
  /// A re-export of another CM library.
  Library(PathBuf),
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
  pub(crate) pathname: PathBuf,
  pub(crate) class: Option<Class>,
}

impl Member {
  pub(crate) fn class(&self) -> Option<Class> {
    self
      .class
      .or_else(|| Class::from_path(self.pathname.as_path()))
  }
}

/// A class of file that may appear in a CM file.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Class {
  Sml,
  CMFile,
  SCGroup,
  SCLibrary,
  MLLex,
  MLYacc,
  MLBurg,
  Rcs,
  Noweb,
}

impl Class {
  fn from_path(path: &Path) -> Option<Self> {
    let ret = match path.extension()?.to_str()? {
      "sig" | "sml" | "fun" => Self::Sml,
      "grm" | "y" => Self::MLYacc,
      "lex" | "l" => Self::MLLex,
      "burg" => Self::MLBurg,
      "cm" => Self::CMFile,
      "sc" => Self::SCGroup,
      "nw" => Self::Noweb,
      _ => {
        if path.as_os_str().to_str()?.ends_with(",v") {
          Self::Rcs
        } else {
          return None;
        }
      }
    };
    Some(ret)
  }
}

impl FromStr for Class {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s.to_ascii_lowercase().as_str() {
      "sml" => Self::Sml,
      "cm" | "cmfile" => Self::CMFile,
      "scgroup" => Self::SCGroup,
      "sclibrary" => Self::SCLibrary,
      "mllex" => Self::MLLex,
      "mlyacc" => Self::MLYacc,
      "mlburg" => Self::MLBurg,
      "rcs" => Self::Rcs,
      "noweb" => Self::Noweb,
      _ => return Err(()),
    };
    Ok(ret)
  }
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Class::Sml => f.write_str("SML"),
      Class::CMFile => f.write_str("CM"),
      Class::SCGroup => f.write_str("SC group"),
      Class::SCLibrary => f.write_str("SC library"),
      Class::MLLex => f.write_str("MLLex"),
      Class::MLYacc => f.write_str("MLYacc"),
      Class::MLBurg => f.write_str("MLBurg"),
      Class::Rcs => f.write_str("RCS"),
      Class::Noweb => f.write_str("noweb"),
    }
  }
}
