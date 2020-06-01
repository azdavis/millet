use crate::ident::Ident;
use crate::source::{Loc, Located, SourceFileId};
use crate::token::{Token, TyVar, ALPHA, OTHER, SYMBOLIC};
use std::fmt;

pub fn get<'s>(file_id: SourceFileId, bs: &'s [u8]) -> Lexer<'s> {
  Lexer {
    file_id,
    bs,
    i: 0,
    line: 1,
    col: 1,
    last_loc: Loc::new(file_id, 1, 1),
  }
}

pub struct Lexer<'s> {
  file_id: SourceFileId,
  bs: &'s [u8],
  i: usize,
  line: usize,
  col: usize,
  last_loc: Loc,
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

impl<'s> Lexer<'s> {
  pub fn next(&mut self) -> Result<Located<Token>, Located<LexError>> {
    let mut comments: usize = 0;
    while let Some(&b) = self.bs.get(self.i) {
      // newline
      if b == b'\n' {
        self.advance_newline();
        continue;
      }
      // comment start
      if b == b'(' && self.bs.get(self.i + 1) == Some(&b'*') {
        self.advance(2);
        comments += 1;
        continue;
      }
      // comment end
      if b == b'*' && self.bs.get(self.i + 1) == Some(&b')') {
        if comments == 0 {
          return Err(self.cur_loc().wrap(LexError::UnmatchedCloseComment));
        }
        self.advance(2);
        comments -= 1;
        continue;
      }
      // inside comment or formatting
      if comments != 0 || is_formatting(b) {
        self.advance(1);
        continue;
      }
      // the actual meat of the impl
      let loc = self.cur_loc();
      self.last_loc = loc;
      return match self.next_impl(b) {
        Ok(t) => Ok(loc.wrap(t)),
        Err(e) => Err(loc.wrap(e)),
      };
    }
    if comments == 0 {
      Ok(self.last_loc.wrap(Token::EOF))
    } else {
      Err(self.cur_loc().wrap(LexError::UnmatchedOpenComment))
    }
  }

  fn next_impl(&mut self, b: u8) -> Result<Token, LexError> {
    // alphanumeric identifiers (including type variables) and alphabetic
    // reserved words
    match alpha_num(b) {
      Some(AlphaNum::Prime) => {
        let start = self.i;
        self.advance(1);
        let b = match self.bs.get(self.i) {
          None => return Err(LexError::IncompleteTypeVar),
          Some(x) => *x,
        };
        let equality = match alpha_num(b) {
          Some(AlphaNum::Prime) => true,
          Some(_) => false,
          None => return Err(LexError::IncompleteTypeVar),
        };
        self.advance(1);
        while let Some(&b) = self.bs.get(self.i) {
          if alpha_num(b).is_none() {
            break;
          }
          self.advance(1);
        }
        let name =
          mk_ident(std::str::from_utf8(&self.bs[start..self.i]).unwrap());
        return Ok(Token::TyVar(TyVar { name, equality }));
      }
      Some(AlphaNum::Alpha) => {
        let start = self.i;
        self.advance(1);
        let mut all_alpha = true;
        while let Some(&b) = self.bs.get(self.i) {
          match alpha_num(b) {
            Some(AlphaNum::Alpha) => {}
            Some(_) => all_alpha = false,
            None => break,
          }
          self.advance(1);
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
        return Ok(Token::AlphaNumId(got));
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
            self.advance(1);
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
            self.advance(1);
            return Ok(Token::DecInt(0));
          }
          Some(x) => *x,
        };
        // word
        if b == b'w' {
          if neg {
            return Err(LexError::InvalidNumConstant);
          }
          self.advance(2);
          let b = match self.bs.get(self.i) {
            None => return Err(LexError::InvalidNumConstant),
            Some(x) => *x,
          };
          return if b == b'x' {
            // hex word
            self.advance(1);
            self.pos_hex_int().map(Token::HexWord)
          } else {
            // decimal word
            self.pos_dec_int().map(Token::DecWord)
          };
        }
        // hex integer
        if b == b'x' {
          self.advance(2);
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
              self.advance(1);
              let exp = self.real_exp()?;
              return mk_real(n, after_dec, exp);
            }
            Some(_) => return mk_real(n, after_dec, 0),
          }
        }
        Some(&b'e') | Some(&b'E') => {
          self.advance(1);
          let exp = self.real_exp()?;
          return mk_real(n, 0.0, exp);
        }
        Some(_) => return Ok(mk_int(n, starts_with_zero)),
      }
    }
    // character constant
    let (b, is_char) = if b == b'#' && self.bs.get(self.i + 1) == Some(&b'"') {
      self.advance(1);
      (b'"', true)
    } else {
      (b, false)
    };
    // string constants
    if b == b'"' {
      self.advance(1);
      let mut str_bs = Vec::new();
      while let Some(&b) = self.bs.get(self.i) {
        match b {
          b'\n' => return Err(LexError::UnclosedStringConstant),
          b'"' => {
            self.advance(1);
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
            self.advance(1);
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
                self.advance(1);
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
                    self.advance(4);
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
                      self.advance(2);
                    }
                    _ => return Err(LexError::InvalidStringConstant),
                  }
                } else if is_formatting(b) {
                  let mut b = b;
                  loop {
                    if b == b'\n' {
                      self.advance_newline();
                    } else {
                      self.advance(1);
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
        self.advance(1);
      }
      return Err(LexError::UnclosedStringConstant);
    }
    // symbolic identifiers and reserved words
    if is_symbolic(b) {
      let start = self.i;
      self.advance(1);
      while let Some(&b) = self.bs.get(self.i) {
        if !is_symbolic(b) {
          break;
        }
        self.advance(1);
      }
      let got = &self.bs[start..self.i];
      for &(tok_bs, ref tok) in SYMBOLIC.iter() {
        if got == tok_bs {
          return Ok(tok.clone());
        }
      }
      let got = mk_ident(std::str::from_utf8(got).unwrap());
      return Ok(Token::SymbolicId(got));
    }
    // other reserved words (that couldn't be mistaken for identifiers)
    for &(tok_bs, ref tok) in OTHER.iter() {
      let tok_n = tok_bs.len();
      if self.bs.get(self.i..self.i + tok_n) == Some(tok_bs) {
        self.advance(tok_n);
        return Ok(tok.clone());
      }
    }
    // unknown byte
    return Err(LexError::UnknownByte(b));
  }

  /// Increase i and col by the given amount.
  fn advance(&mut self, n: usize) {
    self.i += n;
    self.col += n;
  }

  fn advance_newline(&mut self) {
    self.i += 1;
    self.col = 1;
    self.line += 1;
  }

  fn cur_loc(&self) -> Loc {
    Loc::new(self.file_id, self.line, self.col)
  }

  fn pos_dec_int(&mut self) -> Result<i32, LexError> {
    let start = self.i;
    while let Some(b) = self.bs.get(self.i) {
      if !b.is_ascii_digit() {
        break;
      }
      self.advance(1);
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
    self.advance(1);
    while let Some(b) = self.bs.get(self.i) {
      if !b.is_ascii_digit() {
        break;
      }
      self.advance(1);
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
      self.advance(1);
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
      self.advance(1);
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
    b'!' | b'%' | b'&' | b'$' | b'#' | b'+' | b'-' | b'/' | b':' | b'<'
    | b'=' | b'>' | b'?' | b'@' | b'\\' | b'~' | b'`' | b'^' | b'|' | b'*' => {
      true
    }
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
  if n <= 0 || starts_with_zero {
    Token::DecInt(n)
  } else {
    Token::MaybeNumLab(n)
  }
}

fn mk_real(
  before_dec: i32,
  after_dec: f64,
  exp: i32,
) -> Result<Token, LexError> {
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
  use super::{get, hex, mk_ident, Loc, SourceFileId, Token};
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

  #[test]
  fn simple() {
    let file_id = SourceFileId::new(0);
    let inp = include_bytes!("../../../tests/simple.sml");
    let mut out = get(file_id, inp);
    let mk = |line, col, tok| Loc::new(file_id, line, col).wrap(tok);
    let mut next = || out.next().unwrap();
    assert_eq!(next(), mk(01, 01, Token::Val));
    assert_eq!(next(), mk(01, 05, Token::AlphaNumId(mk_ident("decInt"))));
    assert_eq!(next(), mk(01, 12, Token::Equal));
    assert_eq!(next(), mk(01, 14, Token::DecInt(123)));
    assert_eq!(next(), mk(02, 01, Token::Val));
    assert_eq!(next(), mk(02, 05, Token::AlphaNumId(mk_ident("hexInt"))));
    assert_eq!(next(), mk(02, 12, Token::Equal));
    assert_eq!(next(), mk(02, 14, Token::HexInt(65278)));
    assert_eq!(next(), mk(04, 01, Token::Val));
    assert_eq!(next(), mk(04, 05, Token::AlphaNumId(mk_ident("decWord"))));
    assert_eq!(next(), mk(04, 13, Token::Equal));
    assert_eq!(next(), mk(04, 15, Token::DecWord(345)));
    assert_eq!(next(), mk(05, 01, Token::Val));
    assert_eq!(next(), mk(05, 05, Token::AlphaNumId(mk_ident("hexWord"))));
    assert_eq!(next(), mk(05, 13, Token::Equal));
    assert_eq!(next(), mk(05, 15, Token::HexWord(48879)));
    assert_eq!(next(), mk(06, 01, Token::Val));
    assert_eq!(next(), mk(06, 05, Token::AlphaNumId(mk_ident("reals"))));
    assert_eq!(next(), mk(06, 11, Token::Equal));
    assert_eq!(next(), mk(06, 13, Token::LSquare));
    assert_eq!(next(), mk(06, 14, Token::Real(0.7)));
    assert_eq!(next(), mk(06, 17, Token::Comma));
    assert_eq!(next(), mk(06, 19, Token::Real(332000.0)));
    assert_eq!(next(), mk(06, 25, Token::Comma));
    assert_eq!(next(), mk(06, 27, Token::Real(0.0000003)));
    assert_eq!(next(), mk(06, 31, Token::RSquare));
    assert_eq!(next(), mk(07, 01, Token::Val));
    assert_eq!(next(), mk(07, 05, Token::AlphaNumId(mk_ident("str"))));
    assert_eq!(next(), mk(07, 09, Token::Equal));
    assert_eq!(next(), mk(07, 11, Token::Str("foo".to_owned())));
    assert_eq!(next(), mk(08, 01, Token::Val));
    assert_eq!(next(), mk(08, 05, Token::SymbolicId(mk_ident("<=>"))));
    assert_eq!(next(), mk(08, 09, Token::Equal));
    assert_eq!(next(), mk(09, 03, Token::Str("bar quz".to_owned())));
    assert_eq!(next(), mk(11, 01, Token::Val));
    assert_eq!(next(), mk(11, 05, Token::AlphaNumId(mk_ident("c"))));
    assert_eq!(next(), mk(11, 07, Token::Equal));
    assert_eq!(next(), mk(11, 09, Token::Char(63)));
    assert_eq!(next(), mk(11, 09, Token::EOF));
  }
}
