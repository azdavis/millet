use crate::types::{
  Class, DescKind, Error, ErrorKind, Export, Member, Namespace, Result, Root, Token,
};
use located::{Located, TextRange};
use std::path::PathBuf;

pub(crate) fn get(tokens: &[Located<Token<'_>>], env: &paths::slash_var_path::Env) -> Result<Root> {
  let mut p = Parser {
    tokens,
    idx: 0,
    last_range: TextRange::default(),
    env,
  };
  root(&mut p)
}

struct Parser<'a> {
  tokens: &'a [Located<Token<'a>>],
  idx: usize,
  last_range: TextRange,
  env: &'a paths::slash_var_path::Env,
}

impl<'a> Parser<'a> {
  fn cur_tok(&self) -> Option<Located<Token<'a>>> {
    self.tokens.get(self.idx).copied()
  }

  fn cur(&self) -> Option<Token<'a>> {
    self.cur_tok().map(|x| x.val)
  }

  fn err<T>(&self, kind: ErrorKind) -> Result<T> {
    Err(Error::new(kind, self.last_range))
  }

  fn string(&self) -> Result<Located<&'a str>> {
    match self.cur_tok() {
      Some(tok) => match tok.val {
        Token::String(s) => Ok(tok.wrap(s)),
        _ => self.err(ErrorKind::ExpectedString),
      },
      _ => self.err(ErrorKind::ExpectedString),
    }
  }

  fn bump(&mut self) {
    if let Some(tok) = self.cur_tok() {
      self.last_range = tok.range;
      self.idx += 1;
    }
  }

  fn eat(&mut self, kind: Token<'static>) -> Result<()> {
    if self.cur() == Some(kind) {
      self.bump();
      Ok(())
    } else {
      self.err(ErrorKind::Expected(kind))
    }
  }
}

fn root(p: &mut Parser<'_>) -> Result<Root> {
  let ret = match p.cur() {
    Some(Token::Alias) => {
      p.bump();
      let s = p.string()?;
      p.bump();
      let path = path(p, s.val)?;
      Root::Alias(s.wrap(path))
    }
    Some(Token::Group) => {
      p.bump();
      let es = exports(p)?;
      let ms = members_tail(p)?;
      Root::Desc(DescKind::Group, es, ms)
    }
    Some(Token::Library) => {
      p.bump();
      let es = exports(p)?;
      let ms = members_tail(p)?;
      if es.is_empty() {
        return p.err(ErrorKind::EmptyExportList);
      }
      Root::Desc(DescKind::Library, es, ms)
    }
    _ => return p.err(ErrorKind::ExpectedDesc),
  };
  Ok(ret)
}

fn exports(p: &mut Parser<'_>) -> Result<Vec<Export>> {
  let mut ret = Vec::<Export>::new();
  loop {
    let tok = p.cur_tok();
    let tok = match tok {
      Some(x) => x,
      None => break,
    };
    let namespace = match tok.val {
      Token::Structure => Namespace::Structure,
      Token::Signature => Namespace::Signature,
      Token::Functor => Namespace::Functor,
      Token::FunSig => Namespace::FunSig,
      Token::Library => {
        p.bump();
        p.eat(Token::LRound)?;
        let s = p.string()?;
        p.bump();
        let pathname = path(p, s.val)?;
        p.eat(Token::RRound)?;
        ret.push(Export::Library(s.wrap(pathname)));
        continue;
      }
      _ => break,
    };
    p.bump();
    let s = p.string()?;
    let name = str_util::Name::new(s.val);
    p.bump();
    ret.push(Export::Regular(tok.wrap(namespace), s.wrap(name)));
  }
  Ok(ret)
}

fn members_tail(p: &mut Parser<'_>) -> Result<Vec<Member>> {
  p.eat(Token::Is)?;
  let mut ret = Vec::<Member>::new();
  loop {
    let tok = p.cur_tok();
    let tok = match tok {
      Some(x) => x,
      None => break,
    };
    let s = match tok.val {
      Token::String(s) => s,
      _ => break,
    };
    p.bump();
    let pathname = tok.wrap(path(p, s)?);
    let class = match p.cur() {
      Some(Token::Colon) => {
        p.bump();
        let s = p.string()?;
        let c = match s.val.parse::<Class>() {
          Ok(c) => c,
          Err(e) => match e {},
        };
        p.bump();
        Some(s.wrap(c))
      }
      _ => None,
    };
    ret.push(Member { pathname, class });
  }
  Ok(ret)
}

fn path(p: &Parser<'_>, s: &str) -> Result<PathBuf> {
  match paths::slash_var_path::get(s, p.env) {
    Ok(x) => Ok(x),
    Err(e) => p.err(ErrorKind::SlashVarPathError(e)),
  }
}
