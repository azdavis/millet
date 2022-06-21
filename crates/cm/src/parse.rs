use crate::types::{Class, DescKind, Error, Export, Member, Name, Namespace, Result, Root, Token};
use path_slash::PathBufExt as _;
use std::path::PathBuf;

pub(crate) fn get(tokens: &[Token<'_>]) -> Result<Root> {
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
      _ => Err(Error::ExpectedString),
    }
  }

  fn bump(&mut self) {
    self.idx += 1
  }

  fn eat(&mut self, tok: Token<'static>) -> Result<()> {
    if self.cur() == Some(tok) {
      self.bump();
      Ok(())
    } else {
      Err(Error::Expected(tok))
    }
  }
}

fn root(p: &mut Parser<'_>) -> Result<Root> {
  let ret = match p.cur() {
    Some(Token::Alias) => {
      p.bump();
      let path = PathBuf::from_slash(p.string()?);
      p.bump();
      Root::Alias(path)
    }
    Some(Token::Group) => {
      p.bump();
      let es = exports(p)?;
      let ms = members_tail(p)?;
      Root::Desc(DescKind::Group, es, ms)
    }
    Some(Token::LibraryUpper) => {
      p.bump();
      let es = exports(p)?;
      let ms = members_tail(p)?;
      if es.is_empty() {
        return Err(Error::EmptyExportList);
      }
      Root::Desc(DescKind::Library, es, ms)
    }
    _ => return Err(Error::ExpectedDesc),
  };
  Ok(ret)
}

fn exports(p: &mut Parser<'_>) -> Result<Vec<Export>> {
  let mut ret = Vec::<Export>::new();
  loop {
    let namespace = match p.cur() {
      Some(Token::Structure) => Namespace::Structure,
      Some(Token::Signature) => Namespace::Signature,
      Some(Token::Functor) => Namespace::Functor,
      Some(Token::FunSig) => Namespace::FunSig,
      Some(Token::LibraryLower) => {
        p.bump();
        p.eat(Token::LRound)?;
        let pathname = PathBuf::from_slash(p.string()?);
        p.bump();
        p.eat(Token::RRound)?;
        ret.push(Export::Library(pathname));
        continue;
      }
      _ => break,
    };
    p.bump();
    let name = Name::new(p.string()?);
    p.bump();
    ret.push(Export::Regular(namespace, name));
  }
  Ok(ret)
}

fn members_tail(p: &mut Parser<'_>) -> Result<Vec<Member>> {
  p.eat(Token::Is)?;
  let mut ret = Vec::<Member>::new();
  while let Some(Token::String(s)) = p.cur() {
    p.bump();
    let pathname = PathBuf::from_slash(s);
    let class = match p.cur() {
      Some(Token::Colon) => {
        p.bump();
        let s = p.string()?;
        let c = match s.parse::<Class>() {
          Ok(c) => c,
          Err(()) => return Err(Error::ExpectedClass),
        };
        p.bump();
        Some(c)
      }
      _ => None,
    };
    ret.push(Member { pathname, class });
  }
  Ok(ret)
}
