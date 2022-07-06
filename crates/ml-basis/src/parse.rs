use crate::types::{BasDec, BasExp, Error, ErrorKind, Name, NamesSeq, Result, Token};
use located::{Located, TextRange};
use path_slash::PathBufExt as _;
use std::path::PathBuf;

pub(crate) fn get(tokens: &[Located<Token<'_>>]) -> Result<BasDec> {
  let mut p = Parser {
    tokens,
    idx: 0,
    last_range: TextRange::default(),
  };
  bas_dec(&mut p)
}

struct Parser<'a> {
  tokens: &'a [Located<Token<'a>>],
  idx: usize,
  last_range: TextRange,
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

  fn name(&self) -> Result<Located<Name>> {
    match self.cur_tok() {
      Some(tok) => match tok.val {
        Token::Name(s) => Ok(tok.wrap(Name::new(s))),
        _ => self.err(ErrorKind::ExpectedName),
      },
      _ => self.err(ErrorKind::ExpectedName),
    }
  }

  fn string(&self) -> Result<Located<&'a str>> {
    match self.cur_tok() {
      Some(tok) => match tok.val {
        Token::String(s) => Ok(tok.wrap(s)),
        _ => self.err(ErrorKind::ExpectedName),
      },
      _ => self.err(ErrorKind::ExpectedName),
    }
  }
}

fn bas_dec(p: &mut Parser<'_>) -> Result<BasDec> {
  let mut ac = Vec::<BasDec>::new();
  while let Some(bd) = bas_dec_one(p)? {
    if p.cur() == Some(Token::Semicolon) {
      p.bump();
    }
    ac.push(bd);
  }
  let ret = if ac.len() == 1 {
    ac.pop().unwrap()
  } else {
    BasDec::Seq(ac)
  };
  Ok(ret)
}

fn bas_dec_one(p: &mut Parser<'_>) -> Result<Option<BasDec>> {
  let tok = match p.cur_tok() {
    Some(x) => x,
    None => return Ok(None),
  };
  let ret = match tok.val {
    Token::Basis => {
      p.bump();
      let binds = and_sep(p, &mut |p| {
        let name = p.name()?;
        p.bump();
        p.eat(Token::Eq)?;
        let be = bas_exp(p)?;
        Ok((name, be))
      })?;
      BasDec::Basis(binds)
    }
    Token::Open => {
      p.bump();
      let mut names = Vec::<Located<Name>>::new();
      while let Ok(name) = p.name() {
        p.bump();
        names.push(name);
      }
      BasDec::Open(names)
    }
    Token::Local => {
      p.bump();
      let local_dec = bas_dec(p)?;
      p.eat(Token::In)?;
      let in_dec = bas_dec(p)?;
      p.eat(Token::End)?;
      BasDec::Local(local_dec.into(), in_dec.into())
    }
    Token::Structure => {
      p.bump();
      BasDec::Structure(names_seq(p)?)
    }
    Token::Signature => {
      p.bump();
      BasDec::Signature(names_seq(p)?)
    }
    Token::Functor => {
      p.bump();
      BasDec::Functor(names_seq(p)?)
    }
    // TODO allow string as well
    Token::BarePath(path) => {
      p.bump();
      BasDec::Path(tok.wrap(PathBuf::from_slash(path)))
    }
    Token::Ann => {
      p.bump();
      let s = p.string()?;
      let s = s.wrap(s.val.to_owned());
      p.bump();
      p.eat(Token::In)?;
      let bd = bas_dec(p)?;
      p.eat(Token::End)?;
      BasDec::Ann(s, bd.into())
    }
    _ => return Ok(None),
  };
  Ok(Some(ret))
}

fn bas_exp(p: &mut Parser<'_>) -> Result<BasExp> {
  let tok = match p.cur_tok() {
    Some(x) => x,
    None => return p.err(ErrorKind::ExpectedBasExp),
  };
  let ret = match tok.val {
    Token::Bas => {
      p.bump();
      let bd = bas_dec(p)?;
      p.eat(Token::End)?;
      BasExp::Bas(bd)
    }
    Token::Name(n) => {
      p.bump();
      BasExp::Name(tok.wrap(Name::new(n)))
    }
    Token::Let => {
      p.bump();
      let bd = bas_dec(p)?;
      p.eat(Token::In)?;
      let be = bas_exp(p)?;
      p.eat(Token::End)?;
      BasExp::Let(bd, be.into())
    }
    _ => return p.err(ErrorKind::ExpectedBasExp),
  };
  Ok(ret)
}

fn and_sep<F, T>(p: &mut Parser<'_>, f: &mut F) -> Result<Vec<T>>
where
  F: FnMut(&mut Parser<'_>) -> Result<T>,
{
  let mut ret = Vec::<T>::new();
  loop {
    ret.push(f(p)?);
    if p.cur() == Some(Token::And) {
      p.bump();
    } else {
      break;
    }
  }
  Ok(ret)
}

fn names_seq(p: &mut Parser<'_>) -> Result<NamesSeq> {
  and_sep(p, &mut |p| {
    let name = p.name()?;
    p.bump();
    let mut other = None::<Located<Name>>;
    if p.cur() == Some(Token::Eq) {
      p.bump();
      other = Some(p.name()?);
      p.bump();
    };
    Ok((name, other))
  })
}
