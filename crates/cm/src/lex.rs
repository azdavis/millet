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

const PUNCTUATION: [(u8, Token<'_>); 3] = [
  (b':', Token::Colon),
  (b'(', Token::LRound),
  (b')', Token::RRound),
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
  if b == b'#' && idx.checked_sub(1).and_then(|i| bs.get(i)) == Some(&b'\n') {
    *idx += 1;
    advance_while(idx, bs, |b| b != b'\n');
    return Ok(None);
  }
  for (tok_b, tok) in PUNCTUATION {
    if b == tok_b {
      *idx += 1;
      return Ok(Some(tok));
    }
  }
  advance_while(idx, bs, |b| {
    !is_whitespace(b) && !matches!(b, b':' | b'(' | b')' | b';')
  });
  let ret = match std::str::from_utf8(&bs[start..*idx]).unwrap() {
    "structure" => Token::Structure,
    "signature" => Token::Signature,
    "functor" => Token::Functor,
    "funsig" => Token::FunSig,
    "Group" | "group" => Token::Group,
    "Library" | "library" => Token::Library,
    "Alias" | "alias" => Token::Alias,
    "Is" | "is" => Token::Is,
    s => Token::String(s),
  };
  Ok(Some(ret))
}
