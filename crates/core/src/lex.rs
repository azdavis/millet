//! Lexical analysis.

use crate::ident::Ident;
use crate::loc::{Loc, Located};
use crate::token::{IdentType, IsNumLab, Token, TyVar, ALPHA, OTHER, SYMBOLIC};
use std::fmt;

pub fn get(bs: &[u8]) -> Result<Lexer, Located<LexError>> {
  Ok(Lexer::new(TokenBuilder::new(bs).build()?))
}

pub struct Lexer {
  ts: std::vec::IntoIter<Located<Token>>,
  last_loc: Loc,
}

impl Lexer {
  fn new(ts: Vec<Located<Token>>) -> Self {
    Self {
      ts: ts.into_iter(),
      last_loc: Loc::new(0, 0),
    }
  }

  pub fn next(&mut self) -> Located<Token> {
    match self.ts.next() {
      Some(x) => {
        self.last_loc = x.loc;
        x
      }
      None => self.last_loc.wrap(Token::EOF),
    }
  }
}

#[derive(Debug)]
pub enum LexError {
  UnmatchedCloseComment,
  UnmatchedOpenComment,
  IncompleteTypeVar,
  UnknownByte(u8),
  InvalidNumConstant,
  UnclosedStringConstant,
  InvalidStringConstant,
  InvalidCharConstant,
}

impl fmt::Display for LexError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UnmatchedCloseComment => write!(f, "unmatched close comment"),
      Self::UnmatchedOpenComment => write!(f, "unmatched open comment"),
      Self::IncompleteTypeVar => write!(f, "incomplete type var"),
      Self::UnknownByte(b) => write!(f, "unknown byte: {}", b),
      Self::InvalidNumConstant => write!(f, "invalid numeric constant"),
      Self::UnclosedStringConstant => write!(f, "unclosed string constant"),
      Self::InvalidStringConstant => write!(f, "invalid string constant"),
      Self::InvalidCharConstant => write!(f, "invalid character constant"),
    }
  }
}

impl std::error::Error for LexError {}

struct TokenBuilder<'s> {
  bs: &'s [u8],
  i: usize,
}

impl<'s> TokenBuilder<'s> {
  fn new(bs: &'s [u8]) -> Self {
    Self { bs, i: 0 }
  }

  fn build(mut self) -> Result<Vec<Located<Token>>, Located<LexError>> {
    let mut comments: usize = 0;
    let mut ts = Vec::new();
    while let Some(&b) = self.bs.get(self.i) {
      // newline
      if b == b'\n' {
        self.i += 1;
        continue;
      }
      // comment start
      if b == b'(' && self.bs.get(self.i + 1) == Some(&b'*') {
        self.i += 2;
        comments += 1;
        continue;
      }
      // comment end
      if b == b'*' && self.bs.get(self.i + 1) == Some(&b')') {
        if comments == 0 {
          return Err(Loc::new(self.i, self.i + 2).wrap(LexError::UnmatchedCloseComment));
        }
        self.i += 2;
        comments -= 1;
        continue;
      }
      // inside comment or formatting
      if comments != 0 || is_formatting(b) {
        self.i += 1;
        continue;
      }
      // the actual meat of the impl
      let start = self.i;
      let ret = self.next_impl(b);
      let end = self.i;
      let loc = Loc::new(start, end);
      match ret {
        Ok(t) => ts.push(loc.wrap(t)),
        Err(e) => return Err(loc.wrap(e)),
      }
    }
    if comments == 0 {
      Ok(ts)
    } else {
      Err(Loc::new(self.i - 1, self.i).wrap(LexError::UnmatchedOpenComment))
    }
  }

  fn next_impl(&mut self, b: u8) -> Result<Token, LexError> {
    // alphanumeric identifiers (including type variables) and alphabetic
    // reserved words
    match alpha_num(b) {
      Some(AlphaNum::Prime) => {
        let start = self.i;
        self.i += 1;
        let b = match self.bs.get(self.i) {
          None => return Err(LexError::IncompleteTypeVar),
          Some(x) => *x,
        };
        let equality = match alpha_num(b) {
          Some(AlphaNum::Prime) => true,
          Some(_) => false,
          None => return Err(LexError::IncompleteTypeVar),
        };
        self.i += 1;
        while let Some(&b) = self.bs.get(self.i) {
          if alpha_num(b).is_none() {
            break;
          }
          self.i += 1;
        }
        let name = mk_ident(std::str::from_utf8(&self.bs[start..self.i]).unwrap());
        return Ok(Token::TyVar(TyVar { name, equality }));
      }
      Some(AlphaNum::Alpha) => {
        let start = self.i;
        self.i += 1;
        let mut all_alpha = true;
        while let Some(&b) = self.bs.get(self.i) {
          match alpha_num(b) {
            Some(AlphaNum::Alpha) => {}
            Some(_) => all_alpha = false,
            None => break,
          }
          self.i += 1;
        }
        let got = &self.bs[start..self.i];
        // small optimization. we only need to check the ALPHA reserved words
        // if this identifier was all alpha.
        if all_alpha {
          for &(tok_bs, ref tok) in ALPHA.iter() {
            if got == tok_bs {
              return Ok(tok.clone());
            }
          }
        }
        let got = mk_ident(std::str::from_utf8(got).unwrap());
        return Ok(Token::Ident(got, IdentType::AlphaNum));
      }
      Some(AlphaNum::NumOrUnderscore) | None => {}
    }
    // numeric constants. this must come before checking for symbolic
    // identifiers and reserved words, since e.g. `~3` does not actually parse
    // as the negation function ~ followed by the integer literal 3, but
    // rather the ~ is part of the integer literal. this contrasts with e.g.
    // the expression `~ 3` which does parse as the negation function ~
    // followed by the integer literal 3. this first part just handles the
    // optional negation symbol.
    let (b, neg) = if b == b'~' {
      match self.bs.get(self.i + 1) {
        None => (b, false),
        Some(&new_b) => {
          if new_b.is_ascii_digit() {
            self.i += 1;
            // fall through. this will immediately enter the below 'if'.
            (new_b, true)
          } else {
            (b, false)
          }
        }
      }
    } else {
      (b, false)
    };
    // this part actually parses the numeric constants
    if b.is_ascii_digit() {
      // a 'special' constant (hex, word, hex word)
      let starts_with_zero = if b == b'0' {
        let b = match self.bs.get(self.i + 1) {
          None => {
            self.i += 1;
            return Ok(Token::DecInt(0, IsNumLab::No));
          }
          Some(x) => *x,
        };
        // word
        if b == b'w' {
          if neg {
            return Err(LexError::InvalidNumConstant);
          }
          self.i += 2;
          let b = match self.bs.get(self.i) {
            None => return Err(LexError::InvalidNumConstant),
            Some(x) => *x,
          };
          return if b == b'x' {
            // hex word
            self.i += 1;
            self.pos_hex_int().map(Token::HexWord)
          } else {
            // decimal word
            self.pos_dec_int().map(Token::DecWord)
          };
        }
        // hex integer
        if b == b'x' {
          self.i += 2;
          let n = self.pos_hex_int()?;
          let n = if neg { -n } else { n };
          return Ok(Token::HexInt(n));
        }
        // at this point, we've just seen '0', we know there are more bytes
        // after the '0', and the first byte after the '0' is neither 'w' nor
        // 'x'. then this is the beginning of either a decimal integer
        // constant or the first decimal integer part of a real constant.
        false
      } else {
        true
      };
      let n = self.pos_dec_int()?;
      let n = if neg { -n } else { n };
      match self.bs.get(self.i) {
        None => return Ok(mk_int(n, starts_with_zero)),
        Some(&b'.') => {
          // no advance, to fulfill requires of real_after_dec
          let after_dec = self.real_after_dec()?;
          match self.bs.get(self.i) {
            None => return mk_real(n, after_dec, 0),
            Some(&b'e') | Some(&b'E') => {
              self.i += 1;
              let exp = self.real_exp()?;
              return mk_real(n, after_dec, exp);
            }
            Some(_) => return mk_real(n, after_dec, 0),
          }
        }
        Some(&b'e') | Some(&b'E') => {
          self.i += 1;
          let exp = self.real_exp()?;
          return mk_real(n, 0.0, exp);
        }
        Some(_) => return Ok(mk_int(n, starts_with_zero)),
      }
    }
    // character constant
    let (b, is_char) = if b == b'#' && self.bs.get(self.i + 1) == Some(&b'"') {
      self.i += 1;
      (b'"', true)
    } else {
      (b, false)
    };
    // string constants
    if b == b'"' {
      self.i += 1;
      let mut str_bs = Vec::new();
      while let Some(&b) = self.bs.get(self.i) {
        match b {
          b'\n' => return Err(LexError::UnclosedStringConstant),
          b'"' => {
            self.i += 1;
            return if is_char {
              if str_bs.len() == 1 {
                let b = str_bs.pop().unwrap();
                Ok(Token::Char(b))
              } else {
                Err(LexError::InvalidCharConstant)
              }
            } else {
              Ok(Token::Str(String::from_utf8(str_bs).unwrap()))
            };
          }
          b'\\' => {
            self.i += 1;
            let b = match self.bs.get(self.i) {
              None => return Err(LexError::UnclosedStringConstant),
              Some(x) => *x,
            };
            match b {
              b'a' => str_bs.push(7),
              b'b' => str_bs.push(8),
              b't' => str_bs.push(9),
              b'n' => str_bs.push(10),
              b'v' => str_bs.push(11),
              b'f' => str_bs.push(12),
              b'r' => str_bs.push(13),
              b'^' => {
                self.i += 1;
                let b = match self.bs.get(self.i) {
                  None => return Err(LexError::UnclosedStringConstant),
                  Some(x) => *x,
                };
                str_bs.push(b - 64);
              }
              b'u' => {
                if self.i + 4 >= self.bs.len() {
                  return Err(LexError::UnclosedStringConstant);
                }
                match (
                  hex(self.bs[self.i + 1]),
                  hex(self.bs[self.i + 2]),
                  hex(self.bs[self.i + 3]),
                  hex(self.bs[self.i + 4]),
                ) {
                  (Some(0), Some(0), Some(d1), Some(d2)) => {
                    str_bs.push(d1 * 16 + d2);
                    self.i += 4;
                  }
                  _ => return Err(LexError::InvalidStringConstant),
                }
              }
              b'"' => str_bs.push(b'"'),
              b'\\' => str_bs.push(b'\\'),
              b => {
                if let Some(d1) = dec(b) {
                  if self.i + 2 >= self.bs.len() {
                    return Err(LexError::UnclosedStringConstant);
                  }
                  match (dec(self.bs[self.i + 1]), dec(self.bs[self.i + 2])) {
                    (Some(d2), Some(d3)) => {
                      str_bs.push((d1 * 10 + d2) * 10 + d3);
                      self.i += 2;
                    }
                    _ => return Err(LexError::InvalidStringConstant),
                  }
                } else if is_formatting(b) {
                  let mut b = b;
                  loop {
                    if b == b'\n' {
                      self.i += 1;
                    } else {
                      self.i += 1;
                    }
                    b = match self.bs.get(self.i) {
                      None => return Err(LexError::UnclosedStringConstant),
                      Some(x) => *x,
                    };
                    if b == b'\\' {
                      break;
                    }
                    if !is_formatting(b) {
                      return Err(LexError::InvalidStringConstant);
                    }
                  }
                } else {
                  return Err(LexError::InvalidStringConstant);
                }
              }
            }
          }
          b => str_bs.push(b),
        }
        self.i += 1;
      }
      return Err(LexError::UnclosedStringConstant);
    }
    // symbolic identifiers and reserved words
    if is_symbolic(b) {
      let start = self.i;
      self.i += 1;
      while let Some(&b) = self.bs.get(self.i) {
        if !is_symbolic(b) {
          break;
        }
        self.i += 1;
      }
      let got = &self.bs[start..self.i];
      for &(tok_bs, ref tok) in SYMBOLIC.iter() {
        if got == tok_bs {
          return Ok(tok.clone());
        }
      }
      let got = mk_ident(std::str::from_utf8(got).unwrap());
      return Ok(Token::Ident(got, IdentType::Symbolic));
    }
    // other reserved words (that couldn't be mistaken for identifiers)
    for &(tok_bs, ref tok) in OTHER.iter() {
      let tok_n = tok_bs.len();
      if self.bs.get(self.i..self.i + tok_n) == Some(tok_bs) {
        self.i += tok_n;
        return Ok(tok.clone());
      }
    }
    // unknown byte
    return Err(LexError::UnknownByte(b));
  }

  fn pos_dec_int(&mut self) -> Result<i32, LexError> {
    let start = self.i;
    while let Some(b) = self.bs.get(self.i) {
      if !b.is_ascii_digit() {
        break;
      }
      self.i += 1;
    }
    if start == self.i {
      return Err(LexError::InvalidNumConstant);
    }
    let n = std::str::from_utf8(&self.bs[start..self.i]).unwrap();
    let n = match i32::from_str_radix(n, 10) {
      Ok(n) => n,
      Err(_) => return Err(LexError::InvalidNumConstant),
    };
    Ok(n)
  }

  // Requires that self.bs[self.i] currently be on a '.'
  fn real_after_dec(&mut self) -> Result<f64, LexError> {
    let start = self.i;
    self.i += 1;
    while let Some(b) = self.bs.get(self.i) {
      if !b.is_ascii_digit() {
        break;
      }
      self.i += 1;
    }
    if start == self.i {
      return Err(LexError::InvalidNumConstant);
    }
    let n = std::str::from_utf8(&self.bs[start..self.i]).unwrap();
    let n: f64 = match n.parse() {
      Ok(n) => n,
      Err(_) => return Err(LexError::InvalidNumConstant),
    };
    Ok(n)
  }

  fn pos_hex_int(&mut self) -> Result<i32, LexError> {
    let start = self.i;
    while let Some(b) = self.bs.get(self.i) {
      if !b.is_ascii_hexdigit() {
        break;
      }
      self.i += 1;
    }
    if start == self.i {
      return Err(LexError::InvalidNumConstant);
    }
    let n = std::str::from_utf8(&self.bs[start..self.i]).unwrap();
    let n = match i32::from_str_radix(n, 16) {
      Ok(n) => n,
      Err(_) => return Err(LexError::InvalidNumConstant),
    };
    Ok(n)
  }

  fn real_exp(&mut self) -> Result<i32, LexError> {
    let b = match self.bs.get(self.i) {
      None => return Err(LexError::InvalidNumConstant),
      Some(x) => *x,
    };
    let neg = if b == b'~' {
      self.i += 1;
      true
    } else {
      false
    };
    let n = self.pos_dec_int()?;
    Ok(if neg { -n } else { n })
  }
}

fn is_symbolic(b: u8) -> bool {
  match b {
    b'!' | b'%' | b'&' | b'$' | b'#' | b'+' | b'-' | b'/' | b':' | b'<' | b'=' | b'>' | b'?'
    | b'@' | b'\\' | b'~' | b'`' | b'^' | b'|' | b'*' => true,
    _ => false,
  }
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

fn is_formatting(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\n' | 12)
}

fn dec(b: u8) -> Option<u8> {
  if b.is_ascii_digit() {
    Some(b - b'0')
  } else {
    None
  }
}

fn hex(b: u8) -> Option<u8> {
  let d = dec(b);
  if d.is_some() {
    return d;
  }
  if matches!(b, b'a'..=b'f') {
    return Some(b - b'a' + 10);
  }
  if matches!(b, b'A'..=b'F') {
    return Some(b - b'A' + 10);
  }
  None
}

fn mk_int(n: i32, starts_with_zero: bool) -> Token {
  // a number could be a NumLab if is positive and doesn't have a leading zero.
  let is_num_lab = if n > 0 && !starts_with_zero {
    IsNumLab::Maybe
  } else {
    IsNumLab::No
  };
  Token::DecInt(n, is_num_lab)
}

fn mk_real(before_dec: i32, after_dec: f64, exp: i32) -> Result<Token, LexError> {
  use std::convert::TryInto as _;
  let before_dec: f64 = match before_dec.try_into() {
    Ok(n) => n,
    Err(_) => return Err(LexError::InvalidNumConstant),
  };
  let exp: f64 = match exp.try_into() {
    Ok(n) => n,
    Err(_) => return Err(LexError::InvalidNumConstant),
  };
  Ok(Token::Real((before_dec + after_dec) * 10_f64.powf(exp)))
}

fn mk_ident(s: &str) -> Ident {
  Ident::new(s.to_owned())
}

#[cfg(test)]
mod tests {
  use super::hex;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_hex() {
    // digit
    assert_eq!(hex(b'0'), Some(0));
    assert_eq!(hex(b'1'), Some(1));
    assert_eq!(hex(b'2'), Some(2));
    assert_eq!(hex(b'3'), Some(3));
    assert_eq!(hex(b'4'), Some(4));
    assert_eq!(hex(b'5'), Some(5));
    assert_eq!(hex(b'6'), Some(6));
    assert_eq!(hex(b'7'), Some(7));
    assert_eq!(hex(b'8'), Some(8));
    assert_eq!(hex(b'9'), Some(9));
    assert_eq!(hex(b'9'), Some(9));
    // lower hex
    assert_eq!(hex(b'a'), Some(10));
    assert_eq!(hex(b'b'), Some(11));
    assert_eq!(hex(b'c'), Some(12));
    assert_eq!(hex(b'd'), Some(13));
    assert_eq!(hex(b'e'), Some(14));
    assert_eq!(hex(b'f'), Some(15));
    // upper hex
    assert_eq!(hex(b'A'), Some(10));
    assert_eq!(hex(b'B'), Some(11));
    assert_eq!(hex(b'C'), Some(12));
    assert_eq!(hex(b'D'), Some(13));
    assert_eq!(hex(b'E'), Some(14));
    assert_eq!(hex(b'F'), Some(15));
    // other
    assert_eq!(hex(b'.'), None);
    assert_eq!(hex(b'-'), None);
    assert_eq!(hex(b'+'), None);
    assert_eq!(hex(b'G'), None);
    assert_eq!(hex(b'*'), None);
    assert_eq!(hex(b'?'), None);
  }
}
