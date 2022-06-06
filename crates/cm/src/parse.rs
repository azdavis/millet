use crate::{lex::Token, types};
use anyhow::{bail, Result};
use path_slash::PathBufExt as _;
use std::path::PathBuf;

pub(crate) fn get(tokens: &[Token<'_>]) -> Result<types::Root> {
  let mut p = Parser { tokens, idx: 0 };
  root(&mut p)
}

struct Parser<'a> {
  tokens: &'a [Token<'a>],
  idx: usize,
}

impl<'a> Parser<'a> {
  fn cur(&self) -> Option<Token<'a>> {
    self.tokens.get(self.idx).copied()
  }

  fn string(&self) -> Result<&'a str> {
    match self.cur() {
      Some(Token::String(s)) => Ok(s),
      _ => bail!("expected a string"),
    }
  }

  fn bump(&mut self) {
    self.idx += 1
  }

  fn eat(&mut self, tok: Token<'_>) -> Result<()> {
    if self.cur() == Some(tok) {
      self.bump();
      Ok(())
    } else {
      bail!("expected `{:?}`", tok)
    }
  }
}

fn root(p: &mut Parser<'_>) -> Result<types::Root> {
  let ret = match p.cur() {
    Some(Token::Alias) => {
      p.bump();
      let path = PathBuf::from_slash(p.string()?);
      p.bump();
      types::Root::Alias(path)
    }
    Some(Token::Group) => {
      p.bump();
      let es = exports(p)?;
      let ms = members_tail(p)?;
      types::Root::Desc(types::DescKind::Group, es, ms)
    }
    Some(Token::Library) => {
      p.bump();
      let es = exports(p)?;
      if es.is_empty() {
        bail!("expected non-empty export list")
      }
      let ms = members_tail(p)?;
      types::Root::Desc(types::DescKind::Library, es, ms)
    }
    _ => bail!("expected `Alias`, `Group`, or `Library`"),
  };
  Ok(ret)
}

fn exports(p: &mut Parser<'_>) -> Result<Vec<types::Export>> {
  let mut ret = Vec::<types::Export>::new();
  loop {
    let namespace = match p.cur() {
      Some(Token::Structure) => types::Namespace::Structure,
      Some(Token::Signature) => types::Namespace::Signature,
      Some(Token::Functor) => types::Namespace::Functor,
      Some(Token::FunSig) => types::Namespace::FunSig,
      _ => break,
    };
    p.bump();
    let name = types::Name::new(p.string()?);
    p.bump();
    ret.push(types::Export { namespace, name });
  }
  Ok(ret)
}

fn members_tail(p: &mut Parser<'_>) -> Result<Vec<types::Member>> {
  p.eat(Token::Is)?;
  let mut ret = Vec::<types::Member>::new();
  while let Some(Token::String(s)) = p.cur() {
    p.bump();
    let pathname = PathBuf::from_slash(s);
    let class = match p.cur() {
      Some(Token::Colon) => {
        p.bump();
        let c = match p.string()?.parse::<types::Class>() {
          Ok(c) => c,
          Err(()) => bail!("expected a class"),
        };
        p.bump();
        Some(c)
      }
      _ => None,
    };
    ret.push(types::Member { pathname, class });
  }
  Ok(ret)
}
