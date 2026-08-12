//! Getting numbers.

use crate::advance_while;

/// The kind of thing we found.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum Kind {
  Int(i64),
  Word(i64),
  Real {
    whole: i64,
    frac: u64,
    /// base 10
    exp: i64,
  },
}

/// A kind of error.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum ErrorKind {
  MissingDigitsInNumLit,
  NegativeWordLit,
}

fn dec_digit(b: u8) -> Option<u8> {
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

fn hex_digit(b: u8) -> Option<u8> {
  let ret = match b {
    b'a' | b'A' => 10,
    b'b' | b'B' => 11,
    b'c' | b'C' => 12,
    b'd' | b'D' => 13,
    b'e' | b'E' => 14,
    b'f' | b'F' => 15,
    _ => dec_digit(b)?,
  };
  Some(ret)
}

fn advance_while_and_update<F>(idx: &mut usize, bs: &[u8], ac: &mut i64, base: i64, mut f: F)
where
  F: FnMut(u8) -> Option<u8>,
{
  advance_while(idx, bs, |b| match f(b) {
    Some(x) => {
      *ac *= base;
      *ac += i64::from(x);
      true
    }
    None => false,
  });
}

/// Get a number from some bytes.
///
/// # Panics
///
/// On internal error.
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
  let mut ac = 0i64;
  if b == b'0' {
    *idx += 1;
    match bs.get(*idx) {
      None => return Kind::Int(ac),
      // word
      Some(&b'w') => {
        *idx += 1;
        let mut base = 10i64;
        let mk_digit = match bs.get(*idx) {
          Some(&b'x') => {
            *idx += 1;
            base = 16;
            hex_digit
          }
          _ => dec_digit,
        };
        let s = *idx;
        advance_while_and_update(idx, bs, &mut ac, base, mk_digit);
        if s == *idx {
          f(*idx, ErrorKind::MissingDigitsInNumLit);
        }
        if neg {
          f(*idx, ErrorKind::NegativeWordLit);
        }
        return Kind::Word(ac);
      }
      // hex int
      Some(&b'x') => {
        *idx += 1;
        let s = *idx;
        advance_while_and_update(idx, bs, &mut ac, 16, hex_digit);
        if s == *idx {
          f(*idx, ErrorKind::MissingDigitsInNumLit);
        }
        return Kind::Int(ac);
      }
      // dec int that happens to start with 0
      Some(_) => {}
    }
  }
  advance_while(idx, bs, |b| b.is_ascii_digit());
  let mut frac = None::<u64>;
  let mut exp = None::<i64>;
  if let Some(&b'.') = bs.get(*idx) {
    *idx += 1;
    let s = *idx;
    let mut ac = 0i64;
    advance_while_and_update(idx, bs, &mut ac, 10, dec_digit);
    if s == *idx {
      f(*idx, ErrorKind::MissingDigitsInNumLit);
    }
    // we should not go negative
    frac = Some(u64::try_from(ac).expect("frac out of range"));
  }
  if let Some(&b'e' | &b'E') = bs.get(*idx) {
    *idx += 1;
    let mut neg_exp = false;
    if bs.get(*idx) == Some(&b'~') {
      *idx += 1;
      neg_exp = true;
    }
    let s = *idx;
    let mut ac = 0i64;
    advance_while_and_update(idx, bs, &mut ac, 10, dec_digit);
    if s == *idx {
      f(*idx, ErrorKind::MissingDigitsInNumLit);
    }
    if neg_exp {
      ac *= -1;
    }
    exp = Some(ac);
  }
  cov_mark::hit("num_suffix");
  advance_while(idx, bs, |b| b.is_ascii_alphanumeric());
  match (frac, exp) {
    (None, None) => Kind::Int(ac),
    (Some(frac), None) => Kind::Real { whole: ac, frac, exp: 0 },
    (None, Some(exp)) => Kind::Real { whole: ac, frac: 0, exp },
    (Some(frac), Some(exp)) => Kind::Real { whole: ac, frac, exp },
  }
}
