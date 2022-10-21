//! Handling SML string escapes and the like.

use crate::is_whitespace;

/// The result of lexing a string.
#[derive(Debug, Default)]
pub struct Res {
  /// The bytes of the string, with escapes replaced.
  pub bytes: Vec<u8>,
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
      b => push_one(res, idx, b),
    }
  }
  Some(())
}

fn get_escape(res: &mut Res, idx: &mut usize, bs: &[u8]) -> Option<()> {
  assert_eq!(bs[*idx], b'\\');
  *idx += 1;
  match *bs.get(*idx)? {
    b'a' => push_one(res, idx, 7),
    b'b' => push_one(res, idx, 8),
    b't' => push_one(res, idx, 9),
    b'n' => push_one(res, idx, 10),
    b'v' => push_one(res, idx, 11),
    b'f' => push_one(res, idx, 12),
    b'r' => push_one(res, idx, 13),
    b'"' => push_one(res, idx, b'"'),
    b'\\' => push_one(res, idx, b'\\'),
    b'^' => {
      *idx += 1;
      let &c = bs.get(*idx)?;
      if (64..=95).contains(&c) {
        push_one(res, idx, c - 64);
      } else {
        res.errors.push((*idx, Error::InvalidEscape));
        *idx += 1;
      }
    }
    b'u' => {
      *idx += 1;
      let a = ascii_hexdigit(*bs.get(*idx)?);
      *idx += 1;
      let b = ascii_hexdigit(*bs.get(*idx)?);
      *idx += 1;
      let c = ascii_hexdigit(*bs.get(*idx)?);
      *idx += 1;
      let d = ascii_hexdigit(*bs.get(*idx)?);
      *idx += 1;
      match (a, b, c, d) {
        (Some(a), Some(b), Some(c), Some(d)) => {
          let fst = (a << 2) | b;
          let snd = (c << 2) | d;
          if fst != 0 {
            res.bytes.push(fst);
          }
          res.bytes.push(snd);
        }
        _ => res.errors.push((*idx, Error::InvalidEscape)),
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
      } else if let Some(mut ac) = ascii_digit(b) {
        for _ in 0..2 {
          *idx += 1;
          let &c = bs.get(*idx)?;
          match ascii_digit(c).and_then(|c| ac.checked_mul(10)?.checked_add(c)) {
            None => res.errors.push((*idx, Error::InvalidEscape)),
            Some(new_ac) => ac = new_ac,
          }
        }
        push_one(res, idx, ac);
      } else {
        res.errors.push((*idx, Error::InvalidEscape));
        *idx += 1;
      }
    }
  }
  Some(())
}

fn ascii_digit(b: u8) -> Option<u8> {
  let ret = match b {
    b'0' => 0,
    b'1' => 1,
    b'2' => 2,
    b'3' => 3,
    b'4' => 4,
    b'5' => 5,
    b'6' => 6,
    b'7' => 7,
    b'8' => 8,
    b'9' => 9,
    _ => return None,
  };
  Some(ret)
}

fn ascii_hexdigit(b: u8) -> Option<u8> {
  let ret = match b {
    b'a' | b'A' => 10,
    b'b' | b'B' => 11,
    b'c' | b'C' => 12,
    b'd' | b'D' => 13,
    b'e' | b'E' => 14,
    b'f' | b'F' => 15,
    _ => ascii_digit(b)?,
  };
  Some(ret)
}

fn push_one(res: &mut Res, idx: &mut usize, b: u8) {
  *idx += 1;
  res.bytes.push(b);
}
