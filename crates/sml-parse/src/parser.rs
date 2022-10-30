//! The parser. A thin wrapper around event-parse, with operator precedence.

use diagnostic_util::{Code, Severity};
use fast_hash::{map_with_capacity, FxHashMap};
use once_cell::sync::Lazy;
use sml_syntax::rowan::TextRange;
use sml_syntax::token::Token;
use sml_syntax::{SyntaxKind as SK, SyntaxNode};
use std::fmt;

pub(crate) use event_parse::{Entered, Exited, Save};

/// A mapping from names to (in)fixities.
pub type FixEnv = FxHashMap<str_util::Name, Infix>;

/// The default infix operators in the std basis.
pub static STD_BASIS: Lazy<FixEnv> = Lazy::new(|| {
  let ops_arr: [(Infix, &[&str]); 6] = [
    (Infix::left(7), &["*", "/", "div", "mod"]),
    (Infix::left(6), &["+", "-", "^"]),
    (Infix::right(5), &["::", "@"]),
    (Infix::left(4), &["=", "<>", ">", ">=", "<", "<="]),
    (Infix::left(3), &[":=", "o"]),
    (Infix::left(0), &["before"]),
  ];
  let mut ret = map_with_capacity(ops_arr.iter().map(|(_, names)| names.len()).sum());
  for (info, names) in ops_arr {
    for &name in names {
      ret.insert(str_util::Name::new(name), info);
    }
  }
  ret
});

/// A event-based parser for SML.
#[derive(Debug)]
pub(crate) struct Parser<'a> {
  inner: event_parse::Parser<'a, SK, ErrorKind>,
  fix_env: &'a mut FixEnv,
}

impl<'a> Parser<'a> {
  pub(crate) fn new(tokens: &'a [Token<'a, SK>], fix_env: &'a mut FixEnv) -> Self {
    Self { inner: event_parse::Parser::new(tokens), fix_env }
  }

  pub(crate) fn enter(&mut self) -> Entered {
    self.inner.enter()
  }

  pub(crate) fn abandon(&mut self, en: Entered) {
    self.inner.abandon(en);
  }

  pub(crate) fn exit(&mut self, en: Entered, kind: SK) -> Exited {
    self.inner.exit(en, kind)
  }

  pub(crate) fn precede(&mut self, ex: Exited) -> Entered {
    self.inner.precede(ex)
  }

  pub(crate) fn peek(&mut self) -> Option<Token<'a, SK>> {
    self.inner.peek()
  }

  pub(crate) fn peek_n(&mut self, n: usize) -> Option<Token<'a, SK>> {
    self.inner.peek_n(n)
  }

  pub(crate) fn bump(&mut self) -> Token<'a, SK> {
    self.inner.bump()
  }

  pub(crate) fn error(&mut self, kind: ErrorKind) {
    self.inner.error(kind);
  }

  pub(crate) fn finish(self) -> (SyntaxNode, Vec<Error>) {
    let mut sink = event_parse::rowan_sink::RowanSink::default();
    self.inner.finish(&mut sink);
    let (a, b) = sink.finish::<sml_syntax::SML>();
    (a, b.into_iter().map(Error).collect())
  }

  pub(crate) fn at(&mut self, kind: SK) -> bool {
    self.inner.at(kind)
  }

  pub(crate) fn at_n(&mut self, n: usize, kind: SK) -> bool {
    self.inner.at_n(n, kind)
  }

  pub(crate) fn eat(&mut self, kind: SK) -> Option<Token<'a, SK>> {
    self.inner.eat(kind)
  }

  pub(crate) fn save(&self) -> Save {
    self.inner.save()
  }

  pub(crate) fn ok_since(&mut self, save: Save) -> bool {
    self.inner.ok_since(save)
  }

  // sml-specific methods //

  pub(crate) fn insert_infix(&mut self, name: &str, info: Infix) {
    self.fix_env.insert(str_util::Name::new(name), info);
  }

  pub(crate) fn get_infix(&mut self, name: &str) -> Option<Infix> {
    self.fix_env.get(name).copied()
  }

  pub(crate) fn is_infix(&mut self, name: &str) -> bool {
    self.fix_env.contains_key(name)
  }

  pub(crate) fn remove_infix(&mut self, name: &str) {
    self.fix_env.remove(name);
  }
}

// sml-specific types //

/// Information about an infix name.
#[derive(Debug, Clone, Copy)]
pub struct Infix {
  /// The precedence.
  pub prec: u16,
  /// The associativity.
  pub assoc: Assoc,
}

impl Infix {
  /// Returns a new Infix with left associativity.
  pub(crate) fn left(prec: u16) -> Self {
    Self { prec, assoc: Assoc::Left }
  }

  /// Returns a new Infix with right associativity.
  pub(crate) fn right(prec: u16) -> Self {
    Self { prec, assoc: Assoc::Right }
  }
}

/// Associativity for infix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Assoc {
  Left,
  Right,
}

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
    }
  }
}

impl event_parse::Expected<SK> for ErrorKind {
  fn expected(kind: SK) -> Self {
    Self::Expected(Expected::Kind(kind))
  }
}

/// A parse error.
#[derive(Debug)]
pub struct Error(event_parse::rowan_sink::Error<ErrorKind>);

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
  Kind(SK),
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
