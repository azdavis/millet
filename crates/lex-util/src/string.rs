//! Handling SML string escapes and the like.

use crate::is_whitespace;

/// The result of lexing a string.
#[derive(Debug, Default)]
pub struct Res {
  /// The actual the string, with escapes replaced, or None if it was not UTF-8.
  pub actual: Option<String>,
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
  let mut st = St::default();
  if get_(&mut st, idx, bs).is_none() {
    st.errors.push((*idx, Error::Unclosed));
  }
  Res { actual: String::from_utf8(st.bytes).ok(), errors: st.errors }
}

#[derive(Debug, Default)]
struct St {
  bytes: Vec<u8>,
  errors: Vec<(usize, Error)>,
}

/// returns None iff there was no matching `"` to close the string.
fn get_(st: &mut St, idx: &mut usize, bs: &[u8]) -> Option<()> {
  loop {
    match *bs.get(*idx)? {
      b'\n' => return None,
      b'"' => {
        *idx += 1;
        break;
      }
      b'\\' => get_escape(st, idx, bs)?,
      b => push_one(st, idx, b),
    }
  }
  Some(())
}

fn get_escape(st: &mut St, idx: &mut usize, bs: &[u8]) -> Option<()> {
  assert_eq!(bs[*idx], b'\\');
  *idx += 1;
  match *bs.get(*idx)? {
    b'a' => push_one(st, idx, 7),
    b'b' => push_one(st, idx, 8),
    b't' => push_one(st, idx, 9),
    b'n' => push_one(st, idx, 10),
    b'v' => push_one(st, idx, 11),
    b'f' => push_one(st, idx, 12),
    b'r' => push_one(st, idx, 13),
    b'"' => push_one(st, idx, b'"'),
    b'\\' => push_one(st, idx, b'\\'),
    b'^' => {
      *idx += 1;
      let &c = bs.get(*idx)?;
      if (64..=95).contains(&c) {
        push_one(st, idx, c - 64);
      } else {
        st.errors.push((*idx, Error::InvalidEscape));
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
            st.bytes.push(fst);
          }
          st.bytes.push(snd);
        }
        _ => st.errors.push((*idx, Error::InvalidEscape)),
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
            st.errors.push((*idx, Error::NonWhitespaceInContinuation));
          }
        }
      } else if let Some(mut ac) = ascii_digit(b) {
        for _ in 0..2 {
          *idx += 1;
          let &c = bs.get(*idx)?;
          match ascii_digit(c).and_then(|c| ac.checked_mul(10)?.checked_add(c)) {
            None => st.errors.push((*idx, Error::InvalidEscape)),
            Some(new_ac) => ac = new_ac,
          }
        }
        push_one(st, idx, ac);
      } else {
        st.errors.push((*idx, Error::InvalidEscape));
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

fn push_one(st: &mut St, idx: &mut usize, b: u8) {
  *idx += 1;
  st.bytes.push(b);
}
