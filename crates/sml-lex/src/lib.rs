//! Lexes a string into tokens.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use diagnostic_util::{Code, Severity};
use lex_util::{advance_while, block_comment, is_whitespace, string};
use sml_syntax::rowan::{TextRange, TextSize};
use sml_syntax::{token::Token, SyntaxKind as SK};
use std::fmt;

/// A lexed input.
#[derive(Debug)]
pub struct Lex<'a> {
  /// The tokens of the input.
  ///
  /// Concatenated in sequence, they form the original input.
  pub tokens: Vec<Token<'a, SK>>,
  /// The errors encountered.
  pub errors: Vec<Error>,
}

/// An error kind.
#[derive(Debug)]
enum ErrorKind {
  InvalidSource,
  UnclosedComment,
  IncompleteTyVar,
  NegativeWordLit,
  WrongLenCharLit,
  MissingDigitsInNumLit,
  String(string::Error),
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErrorKind::InvalidSource => f.write_str("invalid source character"),
      ErrorKind::UnclosedComment => f.write_str("unclosed comment"),
      ErrorKind::IncompleteTyVar => f.write_str("incomplete type variable"),
      ErrorKind::String(string::Error::Unclosed) => f.write_str("unclosed string literal"),
      ErrorKind::NegativeWordLit => f.write_str("negative word literal"),
      ErrorKind::WrongLenCharLit => f.write_str("character literal must have length 1"),
      ErrorKind::MissingDigitsInNumLit => f.write_str("missing digits in number literal"),
      ErrorKind::String(string::Error::InvalidEscape) => f.write_str("invalid string escape"),
      ErrorKind::String(string::Error::NonWhitespaceInContinuation) => {
        f.write_str("non-whitespace in string continuation")
      }
    }
  }
}

/// An error encountered when lexing.
#[derive(Debug)]
pub struct Error {
  range: TextRange,
  kind: ErrorKind,
}

impl Error {
  /// Returns the range for this.
  #[must_use]
  pub fn range(&self) -> TextRange {
    self.range
  }

  /// Returns a value that displays the message.
  #[must_use]
  pub fn display(&self) -> impl fmt::Display + '_ {
    &self.kind
  }

  /// Returns the code for this.
  #[must_use]
  pub fn code(&self) -> Code {
    match self.kind {
      ErrorKind::InvalidSource => Code::n(2001),
      ErrorKind::UnclosedComment => Code::n(2002),
      ErrorKind::IncompleteTyVar => Code::n(2003),
      ErrorKind::String(string::Error::Unclosed) => Code::n(2004),
      ErrorKind::NegativeWordLit => Code::n(2005),
      ErrorKind::WrongLenCharLit => Code::n(2006),
      ErrorKind::MissingDigitsInNumLit => Code::n(2007),
      ErrorKind::String(string::Error::InvalidEscape) => Code::n(2008),
      ErrorKind::String(string::Error::NonWhitespaceInContinuation) => Code::n(2009),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    Severity::Error
  }
}

/// Returns a [`Lex`] of the input.
///
/// # Panics
///
/// If the lexer failed to advance (an internal error).
#[must_use]
pub fn get(s: &str) -> Lex<'_> {
  let bs = s.as_bytes();
  let mut tokens = Vec::new();
  let mut cx = Cx::default();
  while cx.i < bs.len() {
    let start = cx.i;
    let kind = go(&mut cx, bs);
    assert!(start < cx.i, "lexer failed to advance");
    let text = std::str::from_utf8(&bs[start..cx.i]).unwrap();
    tokens.push(Token { kind, text });
  }
  Lex { tokens, errors: cx.errors }
}

/// The context.
#[derive(Default)]
struct Cx {
  errors: Vec<Error>,
  i: usize,
}

/// requires `bs` is a valid `&str`. if `start` is the value of `cx.i` on entry to this function,
/// this returns `sk` and updates `cx.i` to `end` such that `bs[start..end]` is a `str` and `sk` is
/// the kind for that `str`.
#[allow(clippy::too_many_lines)]
fn go(cx: &mut Cx, bs: &[u8]) -> SK {
  let b = bs[cx.i];
  let start = cx.i;
  // block comments
  match block_comment::get(&mut cx.i, b, bs) {
    Ok(None) => {}
    Ok(Some(block_comment::Consumed)) => return SK::BlockComment,
    Err(block_comment::UnclosedError) => {
      err(cx, start, ErrorKind::UnclosedComment);
      return SK::BlockComment;
    }
  }
  // whitespace
  if is_whitespace(b) {
    cx.i += 1;
    advance_while(&mut cx.i, bs, is_whitespace);
    return SK::Whitespace;
  }
  // alphanumeric identifiers (include type variables) and keywords
  match alpha_num(b) {
    Some(AlphaNum::Prime) => {
      cx.i += 1;
      advance_while(&mut cx.i, bs, |b| alpha_num(b).is_some());
      if start + 1 == cx.i {
        err(cx, start, ErrorKind::IncompleteTyVar);
      }
      return SK::TyVar;
    }
    Some(AlphaNum::Alpha) => {
      cx.i += 1;
      advance_while(&mut cx.i, bs, |b| alpha_num(b).is_some());
      return SK::keyword(&bs[start..cx.i]).unwrap_or(SK::Name);
    }
    Some(AlphaNum::NumOrUnderscore) | None => {}
  }
  // num lit. note e.g. `~3` is one token but `~ 3` is two
  if b.is_ascii_digit() || (b == b'~' && bs.get(cx.i + 1).map_or(false, u8::is_ascii_digit)) {
    let neg = b == b'~';
    let b = if neg {
      cx.i += 1;
      bs[cx.i]
    } else {
      b
    };
    if b == b'0' {
      cx.i += 1;
      match bs.get(cx.i) {
        None => return SK::IntLit,
        // word
        Some(&b'w') => {
          cx.i += 1;
          let valid_digit = match bs.get(cx.i) {
            Some(&b'x') => {
              cx.i += 1;
              u8::is_ascii_hexdigit
            }
            _ => u8::is_ascii_digit,
          };
          let s = cx.i;
          advance_while(&mut cx.i, bs, |b| valid_digit(&b));
          if s == cx.i {
            err(cx, start, ErrorKind::MissingDigitsInNumLit);
          }
          if neg {
            err(cx, start, ErrorKind::NegativeWordLit);
          }
          return SK::WordLit;
        }
        // hex int
        Some(&b'x') => {
          cx.i += 1;
          let s = cx.i;
          advance_while(&mut cx.i, bs, |b| b.is_ascii_hexdigit());
          if s == cx.i {
            err(cx, start, ErrorKind::MissingDigitsInNumLit);
          }
          return SK::IntLit;
        }
        // dec int that happens to start with 0
        Some(_) => {}
      }
    }
    advance_while(&mut cx.i, bs, |b| b.is_ascii_digit());
    let mut kind = SK::IntLit;
    if let Some(&b'.') = bs.get(cx.i) {
      kind = SK::RealLit;
      cx.i += 1;
      let s = cx.i;
      advance_while(&mut cx.i, bs, |b| b.is_ascii_digit());
      if s == cx.i {
        err(cx, start, ErrorKind::MissingDigitsInNumLit);
      }
    }
    if let Some(&b'e' | &b'E') = bs.get(cx.i) {
      kind = SK::RealLit;
      cx.i += 1;
      if bs.get(cx.i) == Some(&b'~') {
        cx.i += 1;
      }
      let s = cx.i;
      advance_while(&mut cx.i, bs, |b| b.is_ascii_digit());
      if s == cx.i {
        err(cx, start, ErrorKind::MissingDigitsInNumLit);
      }
    }
    // @test(misc::num_suffix)
    advance_while(&mut cx.i, bs, |b| b.is_ascii_alphanumeric());
    return kind;
  }
  // string lit
  if b == b'"' {
    get_string(start, cx, bs);
    return SK::StringLit;
  }
  // char lit
  if b == b'#' && bs.get(cx.i + 1) == Some(&b'"') {
    cx.i += 1;
    if get_string(start, cx, bs) != 1 {
      err(cx, start, ErrorKind::WrongLenCharLit);
    }
    return SK::CharLit;
  }
  // symbolic identifiers. must come before punctuation...
  if is_symbolic(b) {
    cx.i += 1;
    advance_while(&mut cx.i, bs, is_symbolic);
    let got = &bs[start..cx.i];
    // ...but we must check if the 'symbolic identifier' was actually a punctuation token. NOTE: this
    // could be a bit quicker if we divide the punctuation tokens into those that 'look like'
    // symbolic identifiers (like `:` and `#`) and those that can't possibly be (like `{` or `,`).
    return SK::PUNCTUATION
      .iter()
      .find_map(|&(sk_text, sk)| (sk_text == got).then_some(sk))
      .unwrap_or(SK::Name);
  }
  // punctuation
  for &(sk_text, sk) in &SK::PUNCTUATION {
    if bs.get(cx.i..cx.i + sk_text.len()) == Some(sk_text) {
      cx.i += sk_text.len();
      return sk;
    }
  }
  // invalid char. go until we find a valid str. this should terminate before
  // cx.i goes past the end of bs because bs comes from a str.
  loop {
    cx.i += 1;
    if std::str::from_utf8(&bs[start..cx.i]).is_ok() {
      break;
    }
  }
  err(cx, start, ErrorKind::InvalidSource);
  SK::Invalid
}

fn get_string(start: usize, cx: &mut Cx, bs: &[u8]) -> usize {
  let res = string::get(&mut cx.i, bs);
  for (idx, e) in res.errors {
    cx.errors.push(Error { range: range(start, idx), kind: ErrorKind::String(e) });
  }
  res.len
}

enum AlphaNum {
  Prime,
  Alpha,
  NumOrUnderscore,
}

fn alpha_num(b: u8) -> Option<AlphaNum> {
  if b == b'\'' {
    Some(AlphaNum::Prime)
  } else if b.is_ascii_alphabetic() {
    Some(AlphaNum::Alpha)
  } else if b.is_ascii_digit() || b == b'_' {
    Some(AlphaNum::NumOrUnderscore)
  } else {
    None
  }
}

fn is_symbolic(b: u8) -> bool {
  matches!(
    b,
    b'!'
      | b'%'
      | b'&'
      | b'$'
      | b'#'
      | b'+'
      | b'-'
      | b'/'
      | b':'
      | b'<'
      | b'='
      | b'>'
      | b'?'
      | b'@'
      | b'\\'
      | b'~'
      | b'`'
      | b'^'
      | b'|'
      | b'*'
  )
}

fn err(cx: &mut Cx, start: usize, kind: ErrorKind) {
  cx.errors.push(Error { range: range(start, cx.i), kind });
}

fn range(start: usize, end: usize) -> TextRange {
  TextRange::new(text_size(start), text_size(end))
}

fn text_size(n: usize) -> TextSize {
  n.try_into().unwrap()
}
