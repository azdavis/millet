//! Getting numbers.

use crate::advance_while;

/// The kind of thing we found.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum Kind {
  Int {
    n: i64,
    hex: bool,
  },
  Word {
    n: u64,
    hex: bool,
  },
  Real {
    neg: bool,
    whole: u64,
    frac_leading_zeroes: u32,
    frac: u64,
    /// base 10
    exp: i64,
  },
}

/// A kind of error.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum Error {
  MissingDigitsInNumLit,
  NegativeWordLit,
  Overflow,
  TrailingSuffix,
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

struct Ac {
  n: Option<i64>,
  leading_zeroes: u32,
  only_zeroes: bool,
}
impl Default for Ac {
  fn default() -> Self {
    Self { n: Some(0), leading_zeroes: 0, only_zeroes: true }
  }
}

impl Ac {
  fn advance_while<F>(&mut self, idx: &mut usize, bs: &[u8], base: i64, mut f: F)
  where
    F: FnMut(u8) -> Option<u8>,
  {
    advance_while(idx, bs, |b| match f(b) {
      Some(x) => {
        if x == 0 {
          if self.only_zeroes {
            self.leading_zeroes += 1;
          }
        } else {
          self.only_zeroes = false;
        }
        self.n =
          self.n.and_then(|ac| ac.checked_mul(base)).and_then(|ac| ac.checked_add(i64::from(x)));
        true
      }
      None => false,
    });
  }
}

fn maybe_note_overflow<F>(x: Option<i64>, idx: usize, f: &mut F) -> i64
where
  F: FnMut(usize, Error),
{
  if let Some(x) = x {
    x
  } else {
    f(idx, Error::Overflow);
    0
  }
}

fn pos(n: i64) -> u64 {
  n.try_into().expect("failed to make pos")
}

fn maybe_neg(neg: bool, n: i64) -> i64 {
  if neg { -n } else { n }
}

/// Get a number from some bytes.
///
/// # Panics
///
/// On internal error.
pub fn get<F>(idx: &mut usize, bs: &[u8], mut f: F) -> Kind
where
  F: FnMut(usize, Error),
{
  let b = bs[*idx];
  let neg = b == b'~';
  let b = if neg {
    *idx += 1;
    bs[*idx]
  } else {
    b
  };
  let mut ac = Ac::default();
  if b == b'0' {
    *idx += 1;
    match bs.get(*idx) {
      None => return Kind::Int { n: ac.n.expect("we know it's zero"), hex: false },
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
        ac.advance_while(idx, bs, base, mk_digit);
        if s == *idx {
          f(*idx, Error::MissingDigitsInNumLit);
        }
        if neg {
          f(*idx, Error::NegativeWordLit);
        }
        return Kind::Word { n: pos(maybe_note_overflow(ac.n, *idx, &mut f)), hex: base == 16 };
      }
      // hex int
      Some(&b'x') => {
        *idx += 1;
        let s = *idx;
        ac.advance_while(idx, bs, 16, hex_digit);
        if s == *idx {
          f(*idx, Error::MissingDigitsInNumLit);
        }
        return Kind::Int { n: maybe_neg(neg, maybe_note_overflow(ac.n, *idx, &mut f)), hex: true };
      }
      // dec int that happens to start with 0
      Some(_) => {}
    }
  }
  ac.advance_while(idx, bs, 10, dec_digit);
  let mut frac = None::<(u64, u32)>;
  let mut exp = None::<i64>;
  if let Some(&b'.') = bs.get(*idx) {
    *idx += 1;
    let s = *idx;
    let mut ac = Ac::default();
    ac.advance_while(idx, bs, 10, dec_digit);
    if s == *idx {
      f(*idx, Error::MissingDigitsInNumLit);
    }
    // we should not go negative
    let res = pos(maybe_note_overflow(ac.n, *idx, &mut f));
    frac = Some((res, ac.leading_zeroes));
  }
  if let Some(&b'e' | &b'E') = bs.get(*idx) {
    *idx += 1;
    let mut neg_exp = false;
    if bs.get(*idx) == Some(&b'~') {
      *idx += 1;
      neg_exp = true;
    }
    let s = *idx;
    let mut ac = Ac::default();
    ac.advance_while(idx, bs, 10, dec_digit);
    if s == *idx {
      f(*idx, Error::MissingDigitsInNumLit);
    }
    if neg_exp {
      ac.n = ac.n.and_then(|ac| ac.checked_mul(-1));
    }
    exp = Some(maybe_note_overflow(ac.n, *idx, &mut f));
  }
  let before = *idx;
  advance_while(idx, bs, |b| b.is_ascii_alphanumeric());
  if before != *idx {
    f(*idx, Error::TrailingSuffix);
  }
  let whole = maybe_note_overflow(ac.n, *idx, &mut f);
  let (frac, lz, exp) = match (frac, exp) {
    (None, None) => return Kind::Int { n: maybe_neg(neg, whole), hex: false },
    (Some((frac, lz)), None) => (frac, lz, 0),
    (None, Some(exp)) => (0, 0, exp),
    (Some((frac, lz)), Some(exp)) => (frac, lz, exp),
  };
  Kind::Real { neg, whole: pos(whole), frac, frac_leading_zeroes: lz, exp }
}
