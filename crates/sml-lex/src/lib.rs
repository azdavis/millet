//! Lexing a string into tokens.

use diagnostic::{Code, Severity};
use lex_util::{advance_while, block_comment, is_whitespace, string};
use sml_syntax::kind::SyntaxKind as SK;
use std::fmt;
use text_size_util::{TextRange, TextSize};
use token::Token;

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

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
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
  let mut st = St::default();
  while st.i < bs.len() {
    let start = st.i;
    let kind = go(&mut st, bs);
    assert!(start < st.i, "lexer failed to advance");
    let text = std::str::from_utf8(&bs[start..st.i]).expect("should get utf-8");
    tokens.push(Token { kind, text });
  }
  Lex { tokens, errors: st.errors }
}

/// The context.
#[derive(Default)]
struct St {
  errors: Vec<Error>,
  i: usize,
}

/// requires `bs` is a valid `&str`. if `start` is the value of `st.i` on entry to this function,
/// this returns `sk` and updates `st.i` to `end` such that `bs[start..end]` is a `str` and `sk` is
/// the kind for that `str`.
#[allow(clippy::too_many_lines)]
fn go(st: &mut St, bs: &[u8]) -> SK {
  let b = bs[st.i];
  let start = st.i;
  // block comments
  match block_comment::get(&mut st.i, b, bs) {
    Ok(None) => {}
    Ok(Some(block_comment::Consumed)) => return SK::BlockComment,
    Err(block_comment::UnclosedError) => {
      err(st, start, ErrorKind::UnclosedComment);
      return SK::BlockComment;
    }
  }
  // whitespace
  if is_whitespace(b) {
    st.i += 1;
    advance_while(&mut st.i, bs, is_whitespace);
    return SK::Whitespace;
  }
  // alphanumeric identifiers (include type variables) and keywords
  match alpha_num(b) {
    Some(AlphaNum::Prime) => {
      st.i += 1;
      advance_while(&mut st.i, bs, |b| alpha_num(b).is_some());
      if start + 1 == st.i {
        err(st, start, ErrorKind::IncompleteTyVar);
      }
      return SK::TyVar;
    }
    Some(AlphaNum::Alpha) => {
      st.i += 1;
      advance_while(&mut st.i, bs, |b| alpha_num(b).is_some());
      return SK::keyword(&bs[start..st.i]).unwrap_or(SK::Name);
    }
    Some(AlphaNum::NumOrUnderscore) | None => {}
  }
  // num lit. note e.g. `~3` is one token but `~ 3` is two
  if b.is_ascii_digit() || (b == b'~' && bs.get(st.i + 1).is_some_and(u8::is_ascii_digit)) {
    let neg = b == b'~';
    let b = if neg {
      st.i += 1;
      bs[st.i]
    } else {
      b
    };
    if b == b'0' {
      st.i += 1;
      match bs.get(st.i) {
        None => return SK::IntLit,
        // word
        Some(&b'w') => {
          st.i += 1;
          let valid_digit = match bs.get(st.i) {
            Some(&b'x') => {
              st.i += 1;
              u8::is_ascii_hexdigit
            }
            _ => u8::is_ascii_digit,
          };
          let s = st.i;
          advance_while(&mut st.i, bs, |b| valid_digit(&b));
          if s == st.i {
            err(st, start, ErrorKind::MissingDigitsInNumLit);
          }
          if neg {
            err(st, start, ErrorKind::NegativeWordLit);
          }
          return SK::WordLit;
        }
        // hex int
        Some(&b'x') => {
          st.i += 1;
          let s = st.i;
          advance_while(&mut st.i, bs, |b| b.is_ascii_hexdigit());
          if s == st.i {
            err(st, start, ErrorKind::MissingDigitsInNumLit);
          }
          return SK::IntLit;
        }
        // dec int that happens to start with 0
        Some(_) => {}
      }
    }
    advance_while(&mut st.i, bs, |b| b.is_ascii_digit());
    let mut kind = SK::IntLit;
    if let Some(&b'.') = bs.get(st.i) {
      kind = SK::RealLit;
      st.i += 1;
      let s = st.i;
      advance_while(&mut st.i, bs, |b| b.is_ascii_digit());
      if s == st.i {
        err(st, start, ErrorKind::MissingDigitsInNumLit);
      }
    }
    if let Some(&b'e' | &b'E') = bs.get(st.i) {
      kind = SK::RealLit;
      st.i += 1;
      if bs.get(st.i) == Some(&b'~') {
        st.i += 1;
      }
      let s = st.i;
      advance_while(&mut st.i, bs, |b| b.is_ascii_digit());
      if s == st.i {
        err(st, start, ErrorKind::MissingDigitsInNumLit);
      }
    }
    cov_mark::hit("num_suffix");
    advance_while(&mut st.i, bs, |b| b.is_ascii_alphanumeric());
    return kind;
  }
  // string lit
  if b == b'"' {
    get_string(st, bs);
    return SK::StringLit;
  }
  // char lit
  if b == b'#' && bs.get(st.i + 1) == Some(&b'"') {
    st.i += 1;
    let s = get_string(st, bs);
    if s.is_some_and(|x| x.len() != 1) {
      err(st, start, ErrorKind::WrongLenCharLit);
    }
    return SK::CharLit;
  }
  // symbolic identifiers. must come before punctuation...
  if is_symbolic(b) {
    st.i += 1;
    advance_while(&mut st.i, bs, is_symbolic);
    let got = &bs[start..st.i];
    // ...but we must check if the 'symbolic identifier' was actually a punctuation token. NOTE: this
    // could be a bit quicker if we divide the punctuation tokens into those that 'look like'
    // symbolic identifiers (like `:` and `#`) and those that can't possibly be (like `{` or `,`).
    return SK::PUNCTUATION
      .iter()
      .find_map(|&(sk_text, sk)| (sk_text == got).then_some(sk))
      .unwrap_or(SK::Name);
  }
  // punctuation
  if let Some(&(sk_bs, sk)) =
    SK::PUNCTUATION.iter().find(|&&(sk_bs, _)| bs.get(st.i..st.i + sk_bs.len()) == Some(sk_bs))
  {
    st.i += sk_bs.len();
    return sk;
  }
  // invalid char. go until we find a valid str. this should terminate before
  // st.i goes past the end of bs because bs comes from a str.
  loop {
    st.i += 1;
    if std::str::from_utf8(&bs[start..st.i]).is_ok() {
      break;
    }
  }
  err(st, start, ErrorKind::InvalidSource);
  SK::Invalid
}

fn get_string(st: &mut St, bs: &[u8]) -> Option<String> {
  let res = string::get(&mut st.i, bs);
  for (idx, e) in res.errors {
    st.errors.push(Error { range: range(idx, idx + 1), kind: ErrorKind::String(e) });
  }
  res.actual
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

fn err(st: &mut St, start: usize, kind: ErrorKind) {
  st.errors.push(Error { range: range(start, st.i), kind });
}

fn range(start: usize, end: usize) -> TextRange {
  TextRange::new(text_size(start), text_size(end))
}

fn text_size(n: usize) -> TextSize {
  n.try_into().unwrap()
}
