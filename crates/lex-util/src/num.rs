//! Getting numbers.

use crate::advance_while;

/// The kind of thing we found.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum Kind {
  Int,
  Word,
  Real,
}

/// A kind of error.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum ErrorKind {
  MissingDigitsInNumLit,
  NegativeWordLit,
}

/// Get a number from some bytes.
pub fn get<F>(idx: &mut usize, bs: &[u8], mut f: F) -> Kind
where
  F: FnMut(usize, ErrorKind),
{
  let b = bs[*idx];
  let neg = b == b'~';
  let b = if neg {
    *idx += 1;
    bs[*idx]
  } else {
    b
  };
  if b == b'0' {
    *idx += 1;
    match bs.get(*idx) {
      None => return Kind::Int,
      // word
      Some(&b'w') => {
        *idx += 1;
        let valid_digit = match bs.get(*idx) {
          Some(&b'x') => {
            *idx += 1;
            u8::is_ascii_hexdigit
          }
          _ => u8::is_ascii_digit,
        };
        let s = *idx;
        advance_while(idx, bs, |b| valid_digit(&b));
        if s == *idx {
          f(*idx, ErrorKind::MissingDigitsInNumLit);
        }
        if neg {
          f(*idx, ErrorKind::NegativeWordLit);
        }
        return Kind::Word;
      }
      // hex int
      Some(&b'x') => {
        *idx += 1;
        let s = *idx;
        advance_while(idx, bs, |b| b.is_ascii_hexdigit());
        if s == *idx {
          f(*idx, ErrorKind::MissingDigitsInNumLit);
        }
        return Kind::Int;
      }
      // dec int that happens to start with 0
      Some(_) => {}
    }
  }
  advance_while(idx, bs, |b| b.is_ascii_digit());
  let mut kind = Kind::Int;
  if let Some(&b'.') = bs.get(*idx) {
    kind = Kind::Real;
    *idx += 1;
    let s = *idx;
    advance_while(idx, bs, |b| b.is_ascii_digit());
    if s == *idx {
      f(*idx, ErrorKind::MissingDigitsInNumLit);
    }
  }
  if let Some(&b'e' | &b'E') = bs.get(*idx) {
    kind = Kind::Real;
    *idx += 1;
    if bs.get(*idx) == Some(&b'~') {
      *idx += 1;
    }
    let s = *idx;
    advance_while(idx, bs, |b| b.is_ascii_digit());
    if s == *idx {
      f(*idx, ErrorKind::MissingDigitsInNumLit);
    }
  }
  cov_mark::hit("num_suffix");
  advance_while(idx, bs, |b| b.is_ascii_alphanumeric());
  kind
}
