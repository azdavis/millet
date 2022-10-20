//! Handling SML string escapes and the like.

use crate::is_whitespace;

/// The result of lexing a string.
#[derive(Debug, Default)]
pub struct Res {
  /// The length of the string.
  pub len: usize,
  /// The errors encountered as pairs of (index where encountered, kind of error).
  pub errors: Vec<(usize, Error)>,
}

/// A kind of string error.
#[derive(Debug)]
pub enum Error {
  /// The string was not closed with a matching `"`.
  Unclosed,
  /// There was an invalid escape.
  InvalidEscape,
  /// There was non-whitespace in a string continuation.
  NonWhitespaceInContinuation,
}

/// Returns information about this SML string.
///
/// # Panics
///
/// If `bs[*idx]` is not a `"` upon entry to this function.
pub fn get(idx: &mut usize, bs: &[u8]) -> Res {
  assert_eq!(bs[*idx], b'"');
  *idx += 1;
  let mut res = Res::default();
  if get_(&mut res, idx, bs).is_none() {
    res.errors.push((*idx, Error::Unclosed));
  }
  res
}

/// returns None iff there was no matching `"` to close the string.
fn get_(res: &mut Res, idx: &mut usize, bs: &[u8]) -> Option<()> {
  loop {
    match *bs.get(*idx)? {
      b'\n' => return None,
      b'"' => {
        *idx += 1;
        break;
      }
      b'\\' => get_escape(res, idx, bs)?,
      _ => *idx += 1,
    }
    res.len += 1;
  }
  Some(())
}

fn get_escape(res: &mut Res, idx: &mut usize, bs: &[u8]) -> Option<()> {
  assert_eq!(bs[*idx], b'\\');
  *idx += 1;
  match *bs.get(*idx)? {
    b'a' | b'b' | b't' | b'n' | b'v' | b'f' | b'r' | b'"' | b'\\' => *idx += 1,
    b'^' => {
      *idx += 1;
      let &c = bs.get(*idx)?;
      if !(64..=95).contains(&c) {
        res.errors.push((*idx, Error::InvalidEscape));
      }
      *idx += 1;
    }
    b'u' => {
      *idx += 1;
      for _ in 0..4 {
        if !bs.get(*idx)?.is_ascii_hexdigit() {
          res.errors.push((*idx, Error::InvalidEscape));
        }
        *idx += 1;
      }
    }
    b => {
      if is_whitespace(b) {
        loop {
          *idx += 1;
          let b = *bs.get(*idx)?;
          if b == b'\\' {
            *idx += 1;
            break;
          }
          if !is_whitespace(b) {
            res.errors.push((*idx, Error::NonWhitespaceInContinuation));
          }
        }
      } else if b.is_ascii_digit() {
        *idx += 1;
        for _ in 0..2 {
          if !bs.get(*idx)?.is_ascii_digit() {
            res.errors.push((*idx, Error::InvalidEscape));
          }
          *idx += 1;
        }
      } else {
        res.errors.push((*idx, Error::InvalidEscape));
        *idx += 1;
      }
    }
  }
  Some(())
}
