use located::{Located, TextRange};
use smol_str::SmolStr;
use std::fmt;
use std::path::PathBuf;

/// std's Result with our Error.
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) enum ErrorKind {
  InvalidSource,
  UnclosedComment,
  Expected(Token<'static>),
  ExpectedBasExp,
  ExpectedName,
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
      ErrorKind::InvalidSource => f.write_str("invalid source character"),
      ErrorKind::UnclosedComment => f.write_str("unclosed block comment"),
      ErrorKind::Expected(tok) => write!(f, "expected `{tok}`"),
      ErrorKind::ExpectedBasExp => f.write_str("expected a basis expression"),
      ErrorKind::ExpectedName => f.write_str("expected a name"),
    }
  }
}

impl std::error::Error for Error {}

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

pub type NamesSeq = Vec<(Located<Name>, Option<Located<Name>>)>;

/// A basis declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum BasDec {
  Basis(Vec<(Located<Name>, BasExp)>),
  Open(Vec<Located<Name>>),
  Local(Box<BasDec>, Box<BasDec>),
  Structure(NamesSeq),
  Signature(NamesSeq),
  Functor(NamesSeq),
  Path(Located<PathBuf>),
  Ann(Located<String>, Box<BasDec>),
  Seq(Vec<BasDec>),
}

/// A basis expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum BasExp {
  Bas(BasDec),
  Name(Located<Name>),
  Let(BasDec, Box<BasExp>),
}
