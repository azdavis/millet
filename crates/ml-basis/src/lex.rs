use crate::types::{Error, ErrorKind, Result, Token};
use located::{mk_text_size, Located, TextRange};
use util::{advance_while, block_comment, is_whitespace};

pub(crate) fn get(s: &str) -> Result<Vec<Located<Token<'_>>>> {
  let bs = s.as_bytes();
  let mut idx = 0usize;
  let mut tokens = Vec::<Located<Token<'_>>>::new();
  while let Some(&b) = bs.get(idx) {
    let old = idx;
    if let Some(val) = token(&mut idx, b, bs)? {
      let range = TextRange::new(mk_text_size(old), mk_text_size(idx));
      tokens.push(Located { val, range });
    }
    assert!(old < idx, "lexer failed to advance");
  }
  Ok(tokens)
}

const PUNCTUATION: [(u8, Token<'_>); 2] = [(b';', Token::Semicolon), (b'=', Token::Eq)];

fn token<'s>(idx: &mut usize, b: u8, bs: &'s [u8]) -> Result<Option<Token<'s>>> {
  let start = *idx;
  match block_comment::get(idx, b, bs) {
    Ok(Some(block_comment::Consumed)) => return Ok(None),
    Ok(None) => {}
    Err(block_comment::UnclosedError) => {
      return Err(Error::new(
        ErrorKind::UnclosedComment,
        TextRange::new(mk_text_size(start), mk_text_size(*idx)),
      ));
    }
  }
  if is_whitespace(b) {
    *idx += 1;
    advance_while(idx, bs, is_whitespace);
    return Ok(None);
  }
  for (tok_b, tok) in PUNCTUATION {
    if b == tok_b {
      *idx += 1;
      return Ok(Some(tok));
    }
  }
  // TODO support all SML string features
  if b == b'"' {
    *idx += 1;
    advance_while(idx, bs, |b| b != b'"');
    *idx += 1;
    return Ok(Some(Token::String(
      std::str::from_utf8(&bs[start..*idx]).unwrap(),
    )));
  }
  advance_while(idx, bs, |b| {
    b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'/' | b'.' | b'$' | b'(' | b')')
  });
  if start == *idx {
    return Err(Error::new(
      ErrorKind::InvalidSource,
      TextRange::empty(mk_text_size(start)),
    ));
  }
  let ret = match std::str::from_utf8(&bs[start..*idx]).unwrap() {
    "signature" => Token::Signature,
    "structure" => Token::Structure,
    "functor" => Token::Functor,
    "basis" => Token::Basis,
    "local" => Token::Local,
    "open" => Token::Open,
    "and" => Token::And,
    "ann" => Token::Ann,
    "bas" => Token::Bas,
    "end" => Token::End,
    "let" => Token::Let,
    "in" => Token::In,
    s => {
      if s.bytes().all(|b| b.is_ascii_alphanumeric()) {
        // this classifies a string like `3` as a name, but that'll be an error later anyway.
        Token::Name(s)
      } else {
        Token::BarePath(s)
      }
    }
  };
  Ok(Some(ret))
}
