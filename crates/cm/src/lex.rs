use anyhow::{bail, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Token<'a> {
  Structure,
  Signature,
  Functor,
  FunSig,
  Group,
  Library,
  Alias,
  Is,
  Colon,
  LRound,
  RRound,
  String(&'a str),
}

pub(crate) fn get(s: &str) -> Result<Vec<Token<'_>>> {
  let bs = s.as_bytes();
  let mut idx = 0usize;
  let mut tokens = Vec::<Token<'_>>::new();
  while let Some(&b) = bs.get(idx) {
    let old = idx;
    if let Some(tok) = token(&mut idx, b, bs)? {
      tokens.push(tok);
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
  match block_comment::get(idx, b, bs) {
    Ok(Some(block_comment::Consumed)) => return Ok(None),
    Ok(None) => {}
    Err(u) => bail!("unmatched {u} comment"),
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
  for (tok_b, tok) in PUNCTUATION {
    if b == tok_b {
      *idx += 1;
      return Ok(Some(tok));
    }
  }
  let start = *idx;
  advance_while(idx, bs, |b| {
    !is_whitespace(b) && !matches!(b, b':' | b'(' | b')' | b';')
  });
  let ret = match std::str::from_utf8(&bs[start..*idx])? {
    "structure" => Token::Structure,
    "signature" => Token::Signature,
    "functor" => Token::Functor,
    "funsig" => Token::FunSig,
    "Group" => Token::Group,
    "Library" => Token::Library,
    "Alias" => Token::Alias,
    "is" => Token::Is,
    s => Token::String(s),
  };
  Ok(Some(ret))
}

fn is_whitespace(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\n' | 12)
}

fn advance_while<P>(idx: &mut usize, bs: &[u8], p: P)
where
  P: Fn(u8) -> bool,
{
  while let Some(&b) = bs.get(*idx) {
    if p(b) {
      *idx += 1;
    } else {
      break;
    }
  }
}
