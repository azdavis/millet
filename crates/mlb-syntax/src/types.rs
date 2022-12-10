//! Types used across the crate.

use std::fmt;
use std::path::{Path, PathBuf};
use str_util::Name;
use text_size_util::{TextRange, WithRange};

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
  SlashVarPathError(paths::slash_var_path::Error),
}

/// An error when processing a ML Basis file.
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
      ErrorKind::InvalidSource => f.write_str("invalid source character"),
      ErrorKind::UnclosedComment => f.write_str("unclosed block comment"),
      ErrorKind::Expected(tok) => write!(f, "expected `{tok}`"),
      ErrorKind::ExpectedBasExp => f.write_str("expected a basis expression"),
      ErrorKind::ExpectedBasDec => f.write_str("expected a basis declaration"),
      ErrorKind::ExpectedName => f.write_str("expected a name"),
      ErrorKind::PathNotSmlOrMlb => f.write_str("path is not SML or MLB"),
      ErrorKind::SlashVarPathError(e) => write!(f, "cannot construct path: {e}"),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Token<'a> {
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

impl fmt::Display for Token<'_> {
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
      Token::String(s) | Token::Name(s) | Token::BarePath(s) => f.write_str(s),
    }
  }
}

/// A sequence of binding names.
pub type NamesSeq = Vec<(WithRange<Name>, Option<WithRange<Name>>)>;

/// A basis declaration.
#[derive(Debug)]
pub enum BasDec {
  /// `basis <name> = <exp>`
  Basis(Vec<(WithRange<Name>, BasExp)>),
  /// `open <name>`
  Open(Vec<WithRange<Name>>),
  /// `local <dec> in <dec> end`
  Local(Box<BasDec>, Box<BasDec>),
  /// `structure <name>`, etc
  Export(Namespace, NamesSeq),
  /// A file path.
  Path(WithRange<ParsedPath>),
  /// `ann <str> in <dec> end`
  Ann(WithRange<String>, Box<BasDec>),
  /// A sequence of declarations.
  Seq(Vec<BasDec>),
}

/// A kind of export.
#[derive(Debug, Clone, Copy)]
pub enum Namespace {
  /// `structure`
  Structure,
  /// `signature`
  Signature,
  /// `functor`
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
  Name(WithRange<Name>),
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
  #[must_use]
  pub fn kind(&self) -> PathKind {
    self.kind
  }

  /// Returns this as a `Path`.
  #[must_use]
  pub fn as_path(&self) -> &Path {
    self.path.as_path()
  }
}
