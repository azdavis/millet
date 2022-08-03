//! Lexes a string into tokens.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

use lex_util::{advance_while, block_comment, is_whitespace};
use std::fmt;
use syntax::rowan::{TextRange, TextSize};
use syntax::{token::Token, SyntaxKind as SK};

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

/// An error encountered when lexing.
#[derive(Debug)]
pub struct Error {
  range: TextRange,
  kind: ErrorKind,
}

impl Error {
  /// Returns the range for this.
  pub fn range(&self) -> TextRange {
    self.range
  }

  /// Returns a value that displays the message.
  pub fn display(&self) -> impl fmt::Display + '_ {
    &self.kind
  }

  /// Returns the code for this.
  pub fn to_code(&self) -> u16 {
    match self.kind {
      ErrorKind::InvalidSource => 2001,
      ErrorKind::UnclosedComment => 2002,
      ErrorKind::IncompleteTyVar => 2003,
      ErrorKind::UnclosedStringLit => 2004,
      ErrorKind::NegativeWordLit => 2005,
      ErrorKind::WrongLenCharLit => 2006,
      ErrorKind::MissingDigitsInNumLit => 2007,
      ErrorKind::InvalidStringEscape => 2008,
      ErrorKind::NonWhitespaceInStringContinuation => 2009,
    }
  }
}

/// An error kind.
#[derive(Debug)]
enum ErrorKind {
  InvalidSource,
  UnclosedComment,
  IncompleteTyVar,
  UnclosedStringLit,
  NegativeWordLit,
  WrongLenCharLit,
  MissingDigitsInNumLit,
  InvalidStringEscape,
  NonWhitespaceInStringContinuation,
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErrorKind::InvalidSource => f.write_str("invalid source character"),
      ErrorKind::UnclosedComment => f.write_str("unclosed comment"),
      ErrorKind::IncompleteTyVar => f.write_str("incomplete type variable"),
      ErrorKind::UnclosedStringLit => f.write_str("unclosed string literal"),
      ErrorKind::NegativeWordLit => f.write_str("negative word literal"),
      ErrorKind::WrongLenCharLit => f.write_str("character literal must have length 1"),
      ErrorKind::MissingDigitsInNumLit => f.write_str("missing digits in number literal"),
      ErrorKind::InvalidStringEscape => f.write_str("invalid string escape"),
      ErrorKind::NonWhitespaceInStringContinuation => {
        f.write_str("non-whitespace in string continuation")
      }
    }
  }
}

/// Returns a [`Lex`] of the input.
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
  Lex {
    tokens,
    errors: cx.errors,
  }
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
            err(cx, start, ErrorKind::MissingDigitsInNumLit)
          }
          if neg {
            err(cx, start, ErrorKind::NegativeWordLit)
          }
          return SK::WordLit;
        }
        // hex int
        Some(&b'x') => {
          cx.i += 1;
          let s = cx.i;
          advance_while(&mut cx.i, bs, |b| b.is_ascii_hexdigit());
          if s == cx.i {
            err(cx, start, ErrorKind::MissingDigitsInNumLit)
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
        err(cx, start, ErrorKind::MissingDigitsInNumLit)
      }
    }
    if let Some(&b'e') | Some(&b'E') = bs.get(cx.i) {
      kind = SK::RealLit;
      cx.i += 1;
      if bs.get(cx.i) == Some(&b'~') {
        cx.i += 1
      }
      let s = cx.i;
      advance_while(&mut cx.i, bs, |b| b.is_ascii_digit());
      if s == cx.i {
        err(cx, start, ErrorKind::MissingDigitsInNumLit)
      }
    }
    return kind;
  }
  // string lit
  if b == b'"' {
    cx.i += 1;
    string(start, cx, bs);
    return SK::StringLit;
  }
  // char lit
  if b == b'#' && bs.get(cx.i + 1) == Some(&b'"') {
    cx.i += 2;
    if string(start, cx, bs) != 1 {
      err(cx, start, ErrorKind::WrongLenCharLit)
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
  for &(sk_text, sk) in SK::PUNCTUATION.iter() {
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

/// requires we just entered a string (so cx.i - 1 is a `"`). returns the number of 'characters' in
/// the string.
fn string(start: usize, cx: &mut Cx, bs: &[u8]) -> usize {
  let mut ret = 0;
  if string_(&mut ret, cx, bs).is_none() {
    err(cx, start, ErrorKind::UnclosedStringLit)
  }
  ret
}

/// returns None iff there was no matching `"` to close the string
fn string_(ret: &mut usize, cx: &mut Cx, bs: &[u8]) -> Option<()> {
  let start = cx.i - 1;
  loop {
    match *bs.get(cx.i)? {
      b'\n' => return None,
      b'"' => {
        cx.i += 1;
        break;
      }
      b'\\' => {
        cx.i += 1;
        match *bs.get(cx.i)? {
          b'a' | b'b' | b't' | b'n' | b'v' | b'f' | b'r' | b'"' | b'\\' => cx.i += 1,
          b'^' => {
            cx.i += 1;
            bs.get(cx.i)?;
            cx.i += 1;
          }
          b'u' => {
            cx.i += 1;
            for _ in 0..4 {
              if !bs.get(cx.i)?.is_ascii_hexdigit() {
                err(cx, start, ErrorKind::InvalidStringEscape);
              }
              cx.i += 1;
            }
          }
          b => {
            if is_whitespace(b) {
              loop {
                cx.i += 1;
                let b = *bs.get(cx.i)?;
                if b == b'\\' {
                  cx.i += 1;
                  break;
                }
                if !is_whitespace(b) {
                  err(cx, start, ErrorKind::NonWhitespaceInStringContinuation);
                }
              }
            } else if b.is_ascii_digit() {
              cx.i += 1;
              for _ in 0..2 {
                if !bs.get(cx.i)?.is_ascii_digit() {
                  err(cx, start, ErrorKind::InvalidStringEscape);
                }
                cx.i += 1;
              }
            } else {
              err(cx, start, ErrorKind::InvalidStringEscape);
              cx.i += 1;
            }
          }
        }
      }
      _ => cx.i += 1,
    }
    *ret += 1;
  }
  Some(())
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
  cx.errors.push(Error {
    range: range(start, cx.i),
    kind,
  });
}

fn range(start: usize, end: usize) -> TextRange {
  TextRange::new(text_size(start), text_size(end))
}

fn text_size(n: usize) -> TextSize {
  n.try_into().unwrap()
}
