//! Parse MLB tokens into a syntax tree.

use crate::types::{
  BasDec, BasExp, Error, ErrorKind, NamesSeq, Namespace, ParsedPath, PathKind, Result, Token,
};
use std::path::Path;
use str_util::Name;
use text_size_util::{TextRange, WithRange};

pub(crate) fn get(
  tokens: &[WithRange<Token<'_>>],
  env: &paths::slash_var_path::Env,
) -> Result<BasDec> {
  let mut p = Parser { tokens, idx: 0, last_range: TextRange::default(), env };
  let ret = bas_dec(&mut p)?;
  match p.cur_tok() {
    None => Ok(ret),
    Some(tok) => Err(Error::new(ErrorKind::ExpectedBasDec, tok.range)),
  }
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

  fn name(&self) -> Result<WithRange<Name>> {
    match self.cur_tok() {
      Some(tok) => match tok.val {
        Token::Name(s) => Ok(tok.wrap(Name::new(s))),
        _ => self.err(ErrorKind::ExpectedName),
      },
      _ => self.err(ErrorKind::ExpectedName),
    }
  }

  fn string(&self) -> Result<WithRange<&'a str>> {
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
  loop {
    let bd = match bas_dec_one(p)? {
      BasDecOne::NoStartTok => break,
      BasDecOne::StdBasisPath => None,
      BasDecOne::Ok(bd) => Some(bd),
    };
    if p.cur() == Some(Token::Semicolon) {
      p.bump();
    }
    if let Some(bd) = bd {
      ac.push(bd);
    }
  }
  let ret = if ac.len() == 1 { ac.pop().unwrap() } else { BasDec::Seq(ac) };
  Ok(ret)
}

enum BasDecOne {
  NoStartTok,
  StdBasisPath,
  Ok(BasDec),
}

fn bas_dec_one(p: &mut Parser<'_>) -> Result<BasDecOne> {
  let tok = match p.cur_tok() {
    Some(x) => x,
    None => return Ok(BasDecOne::NoStartTok),
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
      let mut names = Vec::<WithRange<Name>>::new();
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
      BasDec::Export(Namespace::Structure, names_seq(p)?)
    }
    Token::Signature => {
      p.bump();
      BasDec::Export(Namespace::Signature, names_seq(p)?)
    }
    Token::Functor => {
      p.bump();
      BasDec::Export(Namespace::Functor, names_seq(p)?)
    }
    // TODO allow string paths as well
    Token::BarePath(path) => {
      p.bump();
      let path = match paths::slash_var_path::get(path, p.env) {
        Ok(x) => x,
        Err(e) => {
          if let paths::slash_var_path::Error::Undefined(var) = &e {
            // ignore the sml lib paths (http://mlton.org/MLBasisPathMap) since they're baked in.
            if var == "SML_LIB" {
              return Ok(BasDecOne::StdBasisPath);
            }
          }
          return p.err(ErrorKind::SlashVarPathError(e));
        }
      };
      let kind = match path_kind(path.as_path()) {
        Some(x) => x,
        None => return p.err(ErrorKind::PathNotSmlOrMlb),
      };
      BasDec::Path(tok.wrap(ParsedPath { kind, path }))
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
    _ => return Ok(BasDecOne::NoStartTok),
  };
  Ok(BasDecOne::Ok(ret))
}

fn path_kind(path: &Path) -> Option<PathKind> {
  let ret = match path.extension()?.to_str()? {
    "sml" | "sig" | "fun" => PathKind::Sml,
    "mlb" => PathKind::Mlb,
    _ => return None,
  };
  Some(ret)
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
    let mut other = None::<WithRange<Name>>;
    if p.cur() == Some(Token::Eq) {
      p.bump();
      other = Some(p.name()?);
      p.bump();
    };
    Ok((name, other))
  })
}
