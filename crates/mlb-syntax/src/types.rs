use located::{Located, TextRange};
use std::fmt;
use std::path::{Path, PathBuf};
use str_util::Name;

/// std's Result with our Error.
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) enum ErrorKind {
  InvalidSource,
  UnclosedComment,
  Expected(Token<'static>),
  ExpectedBasExp,
  ExpectedBasDec,
  ExpectedName,
  PathNotSmlOrMlb,
}

/// An error when processing a ML Basis file.
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
      ErrorKind::InvalidSource => f.write_str("invalid source character"),
      ErrorKind::UnclosedComment => f.write_str("unclosed block comment"),
      ErrorKind::Expected(tok) => write!(f, "expected `{tok}`"),
      ErrorKind::ExpectedBasExp => f.write_str("expected a basis expression"),
      ErrorKind::ExpectedBasDec => f.write_str("expected a basis declaration"),
      ErrorKind::ExpectedName => f.write_str("expected a name"),
      ErrorKind::PathNotSmlOrMlb => f.write_str("path is not SML or MLB"),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
  Signature,
  Structure,
  Functor,
  Basis,
  Local,
  Open,
  And,
  Ann,
  Bas,
  End,
  Let,
  In,
  Semicolon,
  Eq,
  String(&'a str),
  Name(&'a str),
  BarePath(&'a str),
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Token::Signature => f.write_str("signature"),
      Token::Structure => f.write_str("structure"),
      Token::Functor => f.write_str("functor"),
      Token::Basis => f.write_str("basis"),
      Token::Local => f.write_str("local"),
      Token::Open => f.write_str("open"),
      Token::And => f.write_str("and"),
      Token::Ann => f.write_str("ann"),
      Token::Bas => f.write_str("bas"),
      Token::End => f.write_str("end"),
      Token::Let => f.write_str("let"),
      Token::In => f.write_str("in"),
      Token::Semicolon => f.write_str(";"),
      Token::Eq => f.write_str("="),
      Token::String(s) => f.write_str(s),
      Token::Name(s) => f.write_str(s),
      Token::BarePath(s) => f.write_str(s),
    }
  }
}

/// A sequence of binding names.
pub type NamesSeq = Vec<(Located<Name>, Option<Located<Name>>)>;

/// A basis declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum BasDec {
  Basis(Vec<(Located<Name>, BasExp)>),
  Open(Vec<Located<Name>>),
  Local(Box<BasDec>, Box<BasDec>),
  Export(Namespace, NamesSeq),
  Path(Located<ParsedPath>),
  Ann(Located<String>, Box<BasDec>),
  Seq(Vec<BasDec>),
}

/// A kind of export.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Namespace {
  Structure,
  Signature,
  Functor,
}

impl fmt::Display for Namespace {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Namespace::Structure => f.write_str("structure"),
      Namespace::Signature => f.write_str("signature"),
      Namespace::Functor => f.write_str("functor"),
    }
  }
}

/// A basis expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum BasExp {
  Bas(BasDec),
  Name(Located<Name>),
  Let(BasDec, Box<BasExp>),
}

/// A kind of path ML Basis knows about.
#[derive(Debug, Clone, Copy)]
pub enum PathKind {
  /// SML paths.
  Sml,
  /// ML Basis paths.
  Mlb,
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
