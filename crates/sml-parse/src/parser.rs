//! The parser. A thin wrapper around event-parse, with operator precedence.

use diagnostic::{Code, Severity};
use sml_syntax::{rowan::TextRange, SyntaxKind};
use std::fmt;

pub(crate) use event_parse::{Entered, Exited};

pub(crate) type Parser<'a> = event_parse::Parser<'a, SyntaxKind, ErrorKind>;

// sml-specific types //

#[derive(Debug)]
pub(crate) enum ErrorKind {
  NotInfix,
  InfixWithoutOp,
  InvalidFixity(std::num::ParseIntError),
  NegativeFixity,
  SameFixityDiffAssoc,
  Expected(Expected),
  UnnecessaryOp,
  UnmatchedClosingDelimiter,
  NeedParensAroundExpHere(ParensExpFlavor),
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErrorKind::NotInfix => f.write_str("non-infix name used as infix"),
      ErrorKind::InfixWithoutOp => f.write_str("infix name used as non-infix without `op`"),
      ErrorKind::InvalidFixity(e) => write!(f, "invalid fixity: {e}"),
      ErrorKind::NegativeFixity => f.write_str("fixity is negative"),
      ErrorKind::SameFixityDiffAssoc => {
        f.write_str("consecutive infix names with same fixity but different associativity")
      }
      ErrorKind::Expected(e) => write!(f, "expected {e}"),
      ErrorKind::UnnecessaryOp => f.write_str("unnecessary `op`"),
      ErrorKind::UnmatchedClosingDelimiter => f.write_str("unmatched closing delimiter"),
      ErrorKind::NeedParensAroundExpHere(flavor) => {
        write!(f, "parentheses required around `{flavor}` expressions here")
      }
    }
  }
}

impl event_parse::Expected<SyntaxKind> for ErrorKind {
  fn expected(kind: SyntaxKind) -> Self {
    Self::Expected(Expected::Kind(kind))
  }
}

#[derive(Debug)]
pub(crate) enum ParensExpFlavor {
  Raise,
  If,
  While,
  Case,
  Fn,
}

impl fmt::Display for ParensExpFlavor {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ParensExpFlavor::Raise => f.write_str("raise"),
      ParensExpFlavor::If => f.write_str("if"),
      ParensExpFlavor::While => f.write_str("while"),
      ParensExpFlavor::Case => f.write_str("case"),
      ParensExpFlavor::Fn => f.write_str("fn"),
    }
  }
}

#[derive(Debug)]
pub(crate) enum Expected {
  Exp,
  Lab,
  Pat,
  Path,
  SigExp,
  StrExp,
  Ty,
  LRoundExpTail,
  Item,
  NameOrInt,
  Kind(SyntaxKind),
}

impl fmt::Display for Expected {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expected::Exp => f.write_str("an expression"),
      Expected::Lab => f.write_str("a label"),
      Expected::Pat => f.write_str("a pattern"),
      Expected::Path => f.write_str("a path"),
      Expected::SigExp => f.write_str("a signature expression"),
      Expected::StrExp => f.write_str("a structure expression"),
      Expected::Ty => f.write_str("a type"),
      Expected::LRoundExpTail => f.write_str("`)`, `,`, or `;`"),
      Expected::Item => f.write_str("a top-level item"),
      Expected::NameOrInt => f.write_str("a name or integer literal"),
      Expected::Kind(k) => k.fmt(f),
    }
  }
}

/// A parse error.
#[derive(Debug)]
pub struct Error(pub(crate) event_parse::rowan_sink::Error<ErrorKind>);

impl Error {
  /// Returns the range for this.
  #[must_use]
  pub fn range(&self) -> TextRange {
    self.0.range
  }

  /// Returns a value that displays the message.
  #[must_use]
  pub fn display(&self) -> impl fmt::Display + '_ {
    &self.0.kind
  }

  /// Returns the code for this.
  #[must_use]
  pub fn code(&self) -> Code {
    match self.0.kind {
      ErrorKind::NotInfix => Code::n(3001),
      ErrorKind::InfixWithoutOp => Code::n(3002),
      ErrorKind::InvalidFixity(_) => Code::n(3003),
      ErrorKind::NegativeFixity => Code::n(3004),
      ErrorKind::SameFixityDiffAssoc => Code::n(3005),
      ErrorKind::Expected(_) => Code::n(3006),
      ErrorKind::UnnecessaryOp => Code::n(3007),
      ErrorKind::UnmatchedClosingDelimiter => Code::n(3008),
      ErrorKind::NeedParensAroundExpHere(_) => Code::n(3009),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    match self.0.kind {
      ErrorKind::UnnecessaryOp => Severity::Warning,
      _ => Severity::Error,
    }
  }
}
