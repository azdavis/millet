//! Parsing.

use crate::ast::{
  Arm, Dec, Exp, FValBind, FValBindCase, Label, Long, Match, Pat, PatRow, Row,
  Ty, TyRow, ValBind,
};
use crate::ident::Ident;
use crate::lex::{LexError, Lexer};
use crate::source::{Loc, Located};
use crate::token::{IdentType, IsNumLab, Token, TyVar};
use std::collections::HashMap;
use std::convert::TryInto as _;
use std::fmt;

pub type Result<T> = std::result::Result<T, Located<ParseError>>;

pub fn get<'s>(lex: Lexer<'s>) -> Result<()> {
  let p = Parser::new(lex);
  Ok(())
}

#[derive(Debug)]
pub enum ParseError {
  LexError(LexError),
  ExpectedButFound(&'static str, &'static str),
  InfixWithoutOp(Ident),
  NotInfix(Ident),
  RealPat,
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::LexError(e) => e.fmt(f),
      Self::ExpectedButFound(exp, fnd) => {
        write!(f, "expected {}, found {}", exp, fnd)
      }
      Self::InfixWithoutOp(id) => {
        write!(f, "infix identifier `{}` used without preceding `op`", id)
      }
      Self::NotInfix(id) => {
        write!(f, "non-infix identifier `{}` used as infix", id)
      }
      Self::RealPat => write!(f, "real constant used as a pattern"),
    }
  }
}

impl std::error::Error for ParseError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::LexError(e) => Some(e),
      Self::ExpectedButFound(..)
      | Self::InfixWithoutOp(..)
      | Self::NotInfix(..)
      | Self::RealPat => None,
    }
  }
}

impl From<LexError> for ParseError {
  fn from(val: LexError) -> Self {
    Self::LexError(val)
  }
}

struct OpInfo {
  num: u32,
  assoc: Assoc,
}

enum Assoc {
  Left,
  Right,
}

struct Parser<'s> {
  lex: Lexer<'s>,
  lookahead: Option<Located<Token>>,
  ops: HashMap<Ident, OpInfo>,
}

// note: the `maybe` family of functions return Result<Option<T>>. these
// functions return:
// - Ok(Some(..)) if they did parse a T
// - Ok(None) if they couldn't parse a T but didn't consume any tokens
// - Err(..) if they couldn't parse a T but did consume tokens

impl<'s> Parser<'s> {
  /// constructs a new Parser.
  fn new(lex: Lexer<'s>) -> Self {
    Self {
      lex,
      lookahead: None,
      ops: HashMap::new(),
    }
  }

  /// gets the next token.
  fn next(&mut self) -> Result<Located<Token>> {
    if let Some(look) = self.lookahead.take() {
      return Ok(look);
    }
    let tok = match self.lex.next() {
      Ok(x) => x,
      Err(e) => return Err(e.loc.wrap(e.val.into())),
    };
    Ok(tok)
  }

  /// if the next token is `tok`, return Ok(()), else error.
  fn eat(&mut self, tok: Token) -> Result<()> {
    let next = self.next()?;
    if next.val == tok {
      Ok(())
    } else {
      self.fail(tok.desc(), next)
    }
  }

  /// backtracks 1 token. this is how lookahead is implemented. the next call to
  /// `next()` will return `tok`. requires that there be no current lookahead.
  fn back(&mut self, tok: Located<Token>) {
    assert!(self.lookahead.is_none());
    self.lookahead = Some(tok);
  }

  /// returns an ExpectedButFound error, where we expected `want` but got `tok`.
  fn fail<T>(&mut self, want: &'static str, tok: Located<Token>) -> Result<T> {
    let err = ParseError::ExpectedButFound(want, tok.val.desc());
    Err(tok.loc.wrap(err))
  }

  fn maybe_at_exp(&mut self) -> Result<Option<Located<Exp<Ident>>>> {
    let tok = self.next()?;
    let exp_loc = tok.loc;
    let exp = match tok.val {
      Token::DecInt(n, _) => Exp::DecInt(n),
      Token::HexInt(n) => Exp::HexInt(n),
      Token::DecWord(n) => Exp::DecWord(n),
      Token::HexWord(n) => Exp::HexWord(n),
      Token::Real(n) => Exp::Real(n),
      Token::Str(s) => Exp::Str(s),
      Token::Char(c) => Exp::Char(c),
      Token::Op => Exp::LongVid(self.long_id(true)?),
      Token::LCurly => {
        let tok = self.next()?;
        if let Token::RCurly = tok.val {
          return Ok(Some(exp_loc.wrap(Exp::Record(Vec::new()))));
        }
        self.back(tok);
        let mut rows = Vec::new();
        loop {
          let lab = self.label()?;
          self.eat(Token::Equal)?;
          let exp = self.exp()?;
          rows.push(Row { lab, exp });
          let tok = self.next()?;
          match tok.val {
            Token::RCurly => break,
            Token::Comma => continue,
            _ => return self.fail("`}` or `,`", tok),
          }
        }
        Exp::Record(rows)
      }
      Token::Pound => Exp::Select(self.label()?),
      Token::LRound => {
        let tok = self.next()?;
        if let Token::RRound = tok.val {
          return Ok(Some(exp_loc.wrap(Exp::Tuple(Vec::new()))));
        }
        self.back(tok);
        let fst = self.exp()?;
        let tok = self.next()?;
        match tok.val {
          Token::RRound => fst.val,
          Token::Comma => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let tok = self.next()?;
              match tok.val {
                Token::RRound => break,
                Token::Comma => continue,
                _ => return self.fail("`)` or `,`", tok),
              }
            }
            Exp::Tuple(exprs)
          }
          Token::Semicolon => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let tok = self.next()?;
              match tok.val {
                Token::RRound => break,
                Token::Semicolon => continue,
                _ => return self.fail("`)` or `;`", tok),
              }
            }
            Exp::Sequence(exprs)
          }
          _ => return self.fail("`)`, `,`, or `;`", tok),
        }
      }
      Token::LSquare => {
        let tok = self.next()?;
        if let Token::RSquare = tok.val {
          return Ok(Some(exp_loc.wrap(Exp::List(Vec::new()))));
        }
        self.back(tok);
        let mut exprs = Vec::new();
        loop {
          exprs.push(self.exp()?);
          let tok = self.next()?;
          match tok.val {
            Token::RSquare => break,
            Token::Comma => continue,
            _ => return self.fail("`]` or `,`", tok),
          }
        }
        Exp::List(exprs)
      }
      Token::Let => {
        let dec = self.dec()?;
        self.eat(Token::In)?;
        let mut exprs = Vec::new();
        loop {
          exprs.push(self.exp()?);
          let tok = self.next()?;
          match tok.val {
            Token::End => break,
            Token::Semicolon => continue,
            _ => return self.fail("`end` or `;`", tok),
          }
        }
        Exp::Let(dec, exprs)
      }
      Token::Ident(..) => Exp::LongVid(self.long_id(false)?),
      _ => {
        // this is the one time we return Ok(None). we need this info to do
        // application expressions correctly.
        self.back(tok);
        return Ok(None);
      }
    };
    Ok(Some(exp_loc.wrap(exp)))
  }

  fn at_exp(&mut self) -> Result<Located<Exp<Ident>>> {
    match self.maybe_at_exp()? {
      Some(x) => Ok(x),
      None => {
        let tok = self.next()?;
        self.fail("an expression", tok)
      }
    }
  }

  fn maybe_long_id(&mut self) -> Result<Option<Long<Ident>>> {
    let mut idents = Vec::new();
    loop {
      let tok = self.next()?;
      if let Token::Ident(id, typ) = tok.val {
        idents.push(tok.loc.wrap(id));
        if let IdentType::Symbolic = typ {
          break;
        }
        let tok = self.next()?;
        if let Token::Dot = tok.val {
          continue;
        } else {
          self.back(tok);
          break;
        }
      }
      return if idents.is_empty() {
        self.back(tok);
        Ok(None)
      } else {
        self.fail("an identifier", tok)
      };
    }
    Ok(Some(Long { idents }))
  }

  fn long_id(&mut self, allow_infix: bool) -> Result<Long<Ident>> {
    let mut ret = match self.maybe_long_id()? {
      Some(x) => x,
      None => {
        let tok = self.next()?;
        return self.fail("an identifier", tok);
      }
    };
    if !allow_infix
      && ret.idents.len() == 1
      && self.ops.contains_key(&ret.idents.first().unwrap().val)
    {
      let id = ret.idents.pop().unwrap();
      Err(id.loc.wrap(ParseError::InfixWithoutOp(id.val)))
    } else {
      Ok(ret)
    }
  }

  fn label(&mut self) -> Result<Located<Label>> {
    let tok = self.next()?;
    let lab = match tok.val {
      Token::DecInt(n, IsNumLab::Maybe) => Label::Num(n.try_into().unwrap()),
      Token::Ident(id, _) => Label::Vid(id),
      _ => return self.fail("a label", tok),
    };
    Ok(tok.loc.wrap(lab))
  }

  // TODO prec
  fn exp(&mut self) -> Result<Located<Exp<Ident>>> {
    let tok = self.next()?;
    let exp_loc = tok.loc;
    let exp = match tok.val {
      Token::Raise => {
        let e = self.exp()?;
        Exp::Raise(e.into())
      }
      Token::If => {
        let e_cond = self.exp()?;
        self.eat(Token::Then)?;
        let e_then = self.exp()?;
        self.eat(Token::Else)?;
        let e_else = self.exp()?;
        Exp::If(e_cond.into(), e_then.into(), e_else.into())
      }
      Token::While => {
        let e_cond = self.exp()?;
        self.eat(Token::Do)?;
        let e_body = self.exp()?;
        Exp::While(e_cond.into(), e_body.into())
      }
      Token::Case => {
        let e_head = self.exp()?;
        self.eat(Token::Of)?;
        let match_ = self.match_()?;
        Exp::Case(e_head.into(), match_)
      }
      Token::Fn => {
        let match_ = self.match_()?;
        Exp::Fn(match_)
      }
      _ => {
        self.back(tok);
        let mut exp = self.at_exp()?;
        while let Some(x) = self.maybe_at_exp()? {
          exp = exp.loc.wrap(Exp::App(exp.into(), x.into()));
        }
        loop {
          let tok = self.next()?;
          exp = exp.loc.wrap(match tok.val {
            Token::Ident(id, _) => {
              let op_info = self.ops.get(&id).unwrap();
              Exp::InfixApp(exp.into(), tok.loc.wrap(id), self.exp()?.into())
            }
            Token::Colon => Exp::Typed(exp.into(), self.ty()?),
            Token::Andalso => Exp::Andalso(exp.into(), self.exp()?.into()),
            Token::Orelse => Exp::Orelse(exp.into(), self.exp()?.into()),
            Token::Handle => Exp::Handle(exp.into(), self.match_()?),
            _ => {
              self.back(tok);
              break;
            }
          });
        }
        exp.val
      }
    };
    Ok(exp_loc.wrap(exp))
  }

  fn match_(&mut self) -> Result<Match<Ident>> {
    let mut arms = Vec::new();
    loop {
      let pat = self.pat()?;
      self.eat(Token::BigArrow)?;
      let exp = self.exp()?;
      arms.push(Arm { pat, exp });
      let tok = self.next()?;
      if let Token::Bar = tok.val {
        continue;
      } else {
        self.back(tok);
        break;
      }
    }
    Ok(Match { arms })
  }

  fn dec(&mut self) -> Result<Located<Dec<Ident>>> {
    let tok = self.next()?;
    let dec = match tok.val {
      Token::Val => {
        let ty_vars = self.ty_var_seq()?;
        let mut val_binds = Vec::new();
        loop {
          let tok = self.next()?;
          let rec = if let Token::Rec = tok.val {
            true
          } else {
            self.back(tok);
            false
          };
          let pat = self.pat()?;
          self.eat(Token::Equal)?;
          let exp = self.exp()?;
          val_binds.push(ValBind { rec, pat, exp });
          let tok = self.next()?;
          if let Token::And = tok.val {
            continue;
          } else {
            self.back(tok);
            break;
          }
        }
        Dec::Val(ty_vars, val_binds)
      }
      Token::Fun => {
        let ty_vars = self.ty_var_seq()?;
        let mut cases = Vec::new();
        let mut binds = Vec::new();
        loop {
          cases.push(self.fval_bind_case()?);
          let tok = self.next()?;
          if let Token::Bar = tok.val {
            continue;
          }
          binds.push(FValBind { cases });
          if let Token::And = tok.val {
            cases = Vec::new();
            continue;
          }
          self.back(tok);
          break;
        }
        Dec::Fun(ty_vars, binds)
      }
      Token::Type => todo!(),
      Token::Datatype => todo!(),
      Token::Abstype => todo!(),
      Token::Exception => todo!(),
      Token::Local => todo!(),
      Token::Open => todo!(),
      Token::Infix => todo!(),
      Token::Infixr => todo!(),
      Token::Nonfix => todo!(),
      _ => return self.fail("a declaration", tok),
    };
    Ok(tok.loc.wrap(dec))
  }

  // NOTE this is not compliant with the spec (page 78): "the parentheses may
  // also be dropped if `: ty` or `=` follows immediately." I can't figure out a
  // way to be both spec compliant and also not require unbounded lookahead.
  fn fval_bind_case(&mut self) -> Result<FValBindCase<Ident>> {
    let mut pats = Vec::new();
    let tok = self.next()?;
    let vid = match tok.val {
      Token::Op => {
        let tok = self.next()?;
        if let Token::Ident(id, _) = tok.val {
          tok.loc.wrap(id)
        } else {
          return self.fail("an identifier", tok);
        }
      }
      Token::LRound => {
        let fst = self.at_pat()?;
        let id = if let Token::Ident(id, _) = tok.val {
          if !self.ops.contains_key(&id) {
            return Err(tok.loc.wrap(ParseError::NotInfix(id)));
          }
          tok.loc.wrap(id)
        } else {
          return self.fail("an identifier", tok);
        };
        let snd = self.at_pat()?;
        pats.push(fst.loc.wrap(Pat::Tuple(vec![fst, snd])));
        id
      }
      Token::Ident(id, _) => {
        if self.ops.contains_key(&id) {
          return Err(tok.loc.wrap(ParseError::InfixWithoutOp(id)));
        } else {
          tok.loc.wrap(id)
        }
      }
      _ => return self.fail("`op`, `(`, or an identifier", tok),
    };
    while let Some(ap) = self.maybe_at_pat()? {
      pats.push(ap);
    }
    let ret_ty = self.maybe_colon_ty()?;
    self.eat(Token::Equal)?;
    let body = self.exp()?;
    Ok(FValBindCase {
      vid,
      pats,
      ret_ty,
      body,
    })
  }

  fn ty_var_seq(&mut self) -> Result<Vec<Located<TyVar<Ident>>>> {
    let tok = self.next()?;
    match tok.val {
      Token::TyVar(ty_var) => return Ok(vec![tok.loc.wrap(ty_var)]),
      Token::LRound => {}
      _ => {
        self.back(tok);
        return Ok(Vec::new());
      }
    }
    let mut ty_vars = Vec::new();
    loop {
      let tok = self.next()?;
      if let Token::TyVar(ty_var) = tok.val {
        ty_vars.push(tok.loc.wrap(ty_var));
      } else {
        return self.fail("a type variable", tok);
      }
      let tok = self.next()?;
      match tok.val {
        Token::RRound => break,
        Token::Comma => continue,
        _ => return self.fail("`)` or `,`", tok),
      }
    }
    Ok(ty_vars)
  }

  fn maybe_at_pat(&mut self) -> Result<Option<Located<Pat<Ident>>>> {
    let tok = self.next()?;
    let pat_loc = tok.loc;
    let pat = match tok.val {
      Token::Underscore => Pat::Wildcard,
      Token::DecInt(n, _) => Pat::DecInt(n),
      Token::HexInt(n) => Pat::HexInt(n),
      Token::DecWord(n) => Pat::DecWord(n),
      Token::HexWord(n) => Pat::HexWord(n),
      Token::Real(..) => return Err(pat_loc.wrap(ParseError::RealPat)),
      Token::Str(s) => Pat::Str(s),
      Token::Char(c) => Pat::Char(c),
      Token::Op => Pat::LongVid(self.long_id(true)?),
      Token::LCurly => {
        let tok = self.next()?;
        if let Token::RCurly = tok.val {
          return Ok(Some(pat_loc.wrap(Pat::Record(Vec::new(), None))));
        }
        self.back(tok);
        let mut rows = Vec::new();
        let mut rest_loc = None;
        loop {
          let tok = self.next()?;
          if let Token::DotDotDot = tok.val {
            rest_loc = Some(tok.loc);
            let tok = self.next()?;
            if let Token::RCurly = tok.val {
              break;
            } else {
              return self.fail("`}`", tok);
            }
          }
          self.back(tok);
          let lab = self.label()?;
          let tok = self.next()?;
          let row = if let Token::Equal = tok.val {
            let pat = self.pat()?;
            PatRow::LabelAndPat(lab, pat)
          } else {
            let vid = match lab.val {
              Label::Vid(x) => lab.loc.wrap(x),
              Label::Num(..) => return self.fail("an identifier", tok),
            };
            let ty = self.maybe_colon_ty()?;
            let as_pat = self.maybe_as_pat()?;
            PatRow::LabelAsVid(vid, ty, as_pat)
          };
          rows.push(row);
          let tok = self.next()?;
          match tok.val {
            Token::RCurly => break,
            Token::Comma => continue,
            _ => return self.fail("`}` or `,`", tok),
          }
        }
        Pat::Record(rows, rest_loc)
      }
      Token::LRound => {
        let tok = self.next()?;
        if let Token::RRound = tok.val {
          return Ok(Some(pat_loc.wrap(Pat::Tuple(Vec::new()))));
        }
        self.back(tok);
        let mut pats = Vec::new();
        loop {
          pats.push(self.pat()?);
          let tok = self.next()?;
          match tok.val {
            Token::RRound => break,
            Token::Comma => continue,
            _ => return self.fail("`)` or `,`", tok),
          }
        }
        if pats.len() == 1 {
          pats.pop().unwrap().val
        } else {
          Pat::Tuple(pats)
        }
      }
      Token::LSquare => {
        let tok = self.next()?;
        if let Token::RSquare = tok.val {
          return Ok(Some(pat_loc.wrap(Pat::List(Vec::new()))));
        }
        self.back(tok);
        let mut pats = Vec::new();
        loop {
          pats.push(self.pat()?);
          let tok = self.next()?;
          match tok.val {
            Token::RSquare => break,
            Token::Comma => continue,
            _ => return self.fail("`]` or `,`", tok),
          }
        }
        Pat::List(pats)
      }
      Token::Ident(..) => Pat::LongVid(self.long_id(false)?),
      _ => {
        self.back(tok);
        return Ok(None);
      }
    };
    Ok(Some(pat_loc.wrap(pat)))
  }

  fn at_pat(&mut self) -> Result<Located<Pat<Ident>>> {
    match self.maybe_at_pat()? {
      Some(x) => Ok(x),
      None => {
        let tok = self.next()?;
        self.fail("a pattern", tok)
      }
    }
  }

  // TODO prec
  fn pat(&mut self) -> Result<Located<Pat<Ident>>> {
    let mut pat = self.at_pat()?;
    if let Pat::LongVid(long_vid) = pat.val {
      pat = pat.loc.wrap(self.pat_long_vid(pat.loc, long_vid)?);
    }
    loop {
      let tok = self.next()?;
      pat = pat.loc.wrap(match tok.val {
        Token::Colon => {
          let ty = self.ty()?;
          Pat::Typed(pat.into(), ty)
        }
        Token::Ident(id, _) => {
          let op_info = self.ops.get(&id).unwrap();
          Pat::InfixCtor(pat.into(), tok.loc.wrap(id), self.pat()?.into())
        }
        _ => {
          self.back(tok);
          break;
        }
      });
    }
    Ok(pat)
  }

  fn pat_long_vid(
    &mut self,
    loc: Loc,
    mut long_vid: Long<Ident>,
  ) -> Result<Pat<Ident>> {
    if long_vid.idents.len() == 1 {
      let ty = self.maybe_colon_ty()?;
      match self.maybe_as_pat()? {
        None => match ty {
          None => {}
          Some(ty) => {
            return Ok(Pat::Typed(loc.wrap(Pat::LongVid(long_vid)).into(), ty))
          }
        },
        Some(as_pat) => {
          let vid = long_vid.idents.pop().unwrap();
          return Ok(Pat::As(vid, ty, as_pat.into()));
        }
      }
    }
    match self.maybe_at_pat()? {
      None => Ok(Pat::LongVid(long_vid)),
      Some(x) => Ok(Pat::Ctor(long_vid, x.into())),
    }
  }

  fn maybe_colon_ty(&mut self) -> Result<Option<Located<Ty<Ident>>>> {
    let tok = self.next()?;
    if let Token::Colon = tok.val {
      Ok(Some(self.ty()?))
    } else {
      self.back(tok);
      Ok(None)
    }
  }

  fn maybe_as_pat(&mut self) -> Result<Option<Located<Pat<Ident>>>> {
    let tok = self.next()?;
    if let Token::As = tok.val {
      Ok(Some(self.pat()?))
    } else {
      self.back(tok);
      Ok(None)
    }
  }

  fn ty(&mut self) -> Result<Located<Ty<Ident>>> {
    match self.maybe_ty()? {
      Some(x) => Ok(x),
      None => {
        let tok = self.next()?;
        self.fail("a type", tok)
      }
    }
  }

  // TODO prec
  fn maybe_ty(&mut self) -> Result<Option<Located<Ty<Ident>>>> {
    let tok = self.next()?;
    let ty_loc = tok.loc;
    let ty = match tok.val {
      Token::TyVar(tv) => Ty::TyVar(tv),
      Token::LCurly => {
        let tok = self.next()?;
        if let Token::RCurly = tok.val {
          return Ok(Some(ty_loc.wrap(Ty::Record(Vec::new()))));
        }
        self.back(tok);
        let mut rows = Vec::new();
        loop {
          let lab = self.label()?;
          self.eat(Token::Colon)?;
          let ty = self.ty()?;
          rows.push(TyRow { lab, ty });
          let tok = self.next()?;
          match tok.val {
            Token::RCurly => break,
            Token::Comma => continue,
            _ => return self.fail("`}` or `,`", tok),
          }
        }
        Ty::Record(rows)
      }
      _ => {
        self.back(tok);
        let mut ty_seq = self.ty_seq()?;
        let long_ty_con = self.maybe_long_id()?;
        let ty = match (ty_seq.len(), long_ty_con) {
          (0, None) => return Ok(None),
          (1, None) => ty_seq.pop().unwrap(),
          (_, None) => {
            let tok = self.next()?;
            return self.fail("an identifier", tok);
          }
          (_, Some(x)) => ty_loc.wrap(Ty::TyCon(ty_seq, x)),
        };
        let mut types = vec![ty];
        loop {
          let tok = self.next()?;
          if let Token::Ident(ref id, _) = tok.val {
            if id.is_star() {
              let ty = self.ty()?;
              types.push(ty);
              continue;
            }
          }
          self.back(tok);
          break;
        }
        let mut ty = if types.len() == 1 {
          types.pop().unwrap()
        } else {
          ty_loc.wrap(Ty::Tuple(types))
        };
        loop {
          let tok = self.next()?;
          if let Token::Arrow = tok.val {
            ty = ty.loc.wrap(Ty::Arrow(ty.into(), self.ty()?.into()));
            continue;
          }
          self.back(tok);
          break;
        }
        ty.val
      }
    };
    Ok(Some(ty_loc.wrap(ty)))
  }

  fn ty_seq(&mut self) -> Result<Vec<Located<Ty<Ident>>>> {
    if let Some(ty) = self.maybe_ty()? {
      return Ok(vec![ty]);
    }
    let tok = self.next()?;
    if let Token::LRound = tok.val {
      //
    } else {
      self.back(tok);
      return Ok(Vec::new());
    }
    let mut types = Vec::new();
    loop {
      let ty = self.ty()?;
      types.push(ty);
      let tok = self.next()?;
      match tok.val {
        Token::RRound => break,
        Token::Comma => continue,
        _ => return self.fail("`)` or `,`", tok),
      }
    }
    Ok(types)
  }
}
