use crate::types::{
  Class, DescKind, Error, ErrorKind, Export, Member, Namespace, Result, Root, Token,
};
use std::path::PathBuf;
use text_size_util::{TextRange, WithRange};

pub(crate) fn get(
  tokens: &[WithRange<Token<'_>>],
  env: &paths::slash_var_path::Env,
) -> Result<Root> {
  let mut p = Parser { tokens, idx: 0, last_range: TextRange::default(), env };
  root(&mut p)
}

struct Parser<'a> {
  tokens: &'a [WithRange<Token<'a>>],
  idx: usize,
  last_range: TextRange,
  env: &'a paths::slash_var_path::Env,
}

impl<'a> Parser<'a> {
  fn cur_tok(&self) -> Option<WithRange<Token<'a>>> {
    self.tokens.get(self.idx).copied()
  }

  fn cur(&self) -> Option<Token<'a>> {
    self.cur_tok().map(|x| x.val)
  }

  fn err<T>(&self, kind: ErrorKind) -> Result<T> {
    Err(Error::new(kind, self.last_range))
  }

  fn string(&self) -> Result<WithRange<&'a str>> {
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
      let path = match path(p, s.val)? {
        Some(x) => x,
        None => return p.err(ErrorKind::AliasWithIgnoredPathVar),
      };
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
    let export = match tok.val {
      Token::Structure => Some(regular(p, tok, Namespace::Structure)?),
      Token::Signature => Some(regular(p, tok, Namespace::Signature)?),
      Token::Functor => Some(regular(p, tok, Namespace::Functor)?),
      Token::FunSig => Some(regular(p, tok, Namespace::FunSig)?),
      Token::Library => {
        p.bump();
        p.eat(Token::LRound)?;
        let s = p.string()?;
        p.bump();
        let pathname = path(p, s.val)?;
        p.eat(Token::RRound)?;
        pathname.map(|pn| Export::Library(s.wrap(pn)))
      }
      Token::Source => {
        p.bump();
        p.eat(Token::LRound)?;
        // TODO also accept one or more filenames?
        p.eat(Token::Minus)?;
        p.eat(Token::RRound)?;
        Some(Export::Source(tok.range))
      }
      Token::Group => {
        p.bump();
        p.eat(Token::LRound)?;
        // TODO also accept one or more filenames?
        p.eat(Token::Minus)?;
        p.eat(Token::RRound)?;
        Some(Export::Group(tok.range))
      }
      _ => break,
    };
    if let Some(export) = export {
      ret.push(export);
    }
  }
  Ok(ret)
}

fn regular(p: &mut Parser<'_>, tok: WithRange<Token<'_>>, ns: Namespace) -> Result<Export> {
  p.bump();
  let s = p.string()?;
  let name = str_util::Name::new(s.val);
  p.bump();
  Ok(Export::Regular(tok.wrap(ns), s.wrap(name)))
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
    let pathname = path(p, s)?;
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
    if let Some(pathname) = pathname {
      ret.push(Member { pathname: tok.wrap(pathname), class });
    }
  }
  Ok(ret)
}

fn path(p: &Parser<'_>, s: &str) -> Result<Option<PathBuf>> {
  match paths::slash_var_path::get(s, p.env) {
    Ok(x) => Ok(Some(x)),
    Err(e) => {
      if let paths::slash_var_path::Error::Undefined(var) = &e {
        if matches!(var.as_str(), "" | "SMLNJ-LIB") {
          return Ok(None);
        }
      }
      p.err(ErrorKind::SlashVarPathError(e))
    }
  }
}
