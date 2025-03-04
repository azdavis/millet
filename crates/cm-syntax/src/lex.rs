//! Lexing CM files.

use crate::types::{Error, ErrorKind, Result, Token};
use lex_util::{advance_while, block_comment, is_whitespace};
use text_size_util::{TextRange, WithRange, mk_text_size};

pub(crate) fn get(s: &str) -> Result<Vec<WithRange<Token<'_>>>> {
  let bs = s.as_bytes();
  let mut idx = 0usize;
  let mut tokens = Vec::<WithRange<Token<'_>>>::new();
  while let Some(&b) = bs.get(idx) {
    let old = idx;
    if let Some(val) = token(&mut idx, b, bs)? {
      let range = TextRange::new(mk_text_size(old), mk_text_size(idx));
      tokens.push(WithRange { val, range });
    }
    assert!(old < idx, "lexer failed to advance");
  }
  Ok(tokens)
}

const PUNCTUATION: [(&[u8], Token<'_>); 6] = [
  (b"...", Token::Dots),
  (b"*", Token::Star),
  (b"-", Token::Minus),
  (b":", Token::Colon),
  (b"(", Token::LRound),
  (b")", Token::RRound),
];

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
  if b == b';' {
    *idx += 1;
    advance_while(idx, bs, |b| b != b'\n');
    return Ok(None);
  }
  if is_whitespace(b) {
    *idx += 1;
    advance_while(idx, bs, is_whitespace);
    return Ok(None);
  }
  // preprocessor (ignored)
  if b == b'#' && idx.checked_sub(1).is_none_or(|i| bs.get(i) == Some(&b'\n')) {
    *idx += 1;
    advance_while(idx, bs, |b| b != b'\n');
    return Ok(None);
  }
  if let Some(&(tok_bs, tok)) =
    PUNCTUATION.iter().find(|&&(tok_bs, _)| bs.get(*idx..*idx + tok_bs.len()) == Some(tok_bs))
  {
    *idx += tok_bs.len();
    return Ok(Some(tok));
  }
  advance_while(idx, bs, |b| !is_whitespace(b) && !matches!(b, b':' | b'(' | b')' | b';'));
  let ret = match std::str::from_utf8(&bs[start..*idx]).unwrap() {
    "structure" => Token::Structure,
    "signature" => Token::Signature,
    "functor" => Token::Functor,
    "funsig" => Token::FunSig,
    "group" | "Group" | "GROUP" => Token::Group,
    "library" | "Library" | "LIBRARY" => Token::Library,
    "source" | "Source" | "SOURCE" => Token::Source,
    "is" | "IS" => Token::Is,
    s => Token::String(s),
  };
  Ok(Some(ret))
}
