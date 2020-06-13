//! Parsing.

use crate::ast::{
  Arm, ConBind, ConDesc, DatBind, DatDesc, Dec, ExBind, ExBindInner, ExDesc, Exp, FValBind,
  FValBindCase, FunBind, Label, Long, Match, Pat, PatRow, Row, SigBind, SigExp, Spec, StrBind,
  StrDec, StrDesc, StrExp, TopDec, Ty, TyBind, TyDesc, TyRow, ValBind, ValDesc,
};
use crate::intern::StrRef;
use crate::lex::Lexer;
use crate::loc::{Loc, Located};
use crate::token::{IdentType, IsNumLab, Token, TyVar};
use std::collections::HashMap;
use std::convert::TryInto as _;

pub type Result<T> = std::result::Result<T, Located<ParseError>>;

pub fn get(lexer: Lexer) -> Result<Vec<Located<TopDec<StrRef>>>> {
  Parser::new(lexer).program()
}

#[derive(Debug)]
pub enum ParseError {
  ExpectedButFound(&'static str, &'static str),
  InfixWithoutOp(StrRef),
  NotInfix(StrRef),
  RealPat,
  NegativeFixity(i32),
}

struct Parser {
  lexer: Lexer,
  lookahead: Option<Located<Token>>,
  ops: HashMap<StrRef, OpInfo>,
}

// note: the `maybe` family of functions return Result<Option<T>>. these functions return:
// - Ok(Some(..)) if they did parse a T
// - Ok(None) if they couldn't parse a T but didn't consume any tokens
// - Err(..) if they couldn't parse a T but did consume tokens

impl Parser {
  /// constructs a new Parser.
  fn new(lexer: Lexer) -> Self {
    Self {
      lexer,
      lookahead: None,
      ops: HashMap::new(),
    }
  }

  /// gets the next token.
  fn next(&mut self) -> Located<Token> {
    if let Some(look) = self.lookahead.take() {
      return look;
    }
    self.lexer.next()
  }

  /// if the next token is `tok`, return `Ok(())`, else return `Err(..)`.
  fn eat(&mut self, tok: Token) -> Result<()> {
    let next = self.next();
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

  fn program(&mut self) -> Result<Vec<Located<TopDec<StrRef>>>> {
    let mut ret = Vec::new();
    while let Some(td) = self.maybe_top_dec()? {
      ret.push(td);
    }
    let tok = self.next();
    if let Token::EOF = tok.val {
      Ok(ret)
    } else {
      self.fail("a top-level declaration", tok)
    }
  }

  fn maybe_top_dec(&mut self) -> Result<Option<Located<TopDec<StrRef>>>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = match tok.val {
      Token::Signature => {
        let mut sig_binds = Vec::new();
        loop {
          let id = self.alpha_num_id()?;
          self.eat(Token::Equal)?;
          let exp = self.sig_exp()?;
          sig_binds.push(SigBind { id, exp });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        TopDec::SigDec(sig_binds)
      }
      Token::Functor => {
        let mut fun_binds = Vec::new();
        loop {
          let fun_id = self.alpha_num_id()?;
          self.eat(Token::LRound)?;
          let str_id = self.alpha_num_id()?;
          self.eat(Token::Colon)?;
          let sig_exp = self.sig_exp()?;
          self.eat(Token::RRound)?;
          self.eat(Token::Equal)?;
          let str_exp = self.str_exp()?;
          fun_binds.push(FunBind {
            fun_id,
            str_id,
            sig_exp,
            str_exp,
          });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        TopDec::FunDec(fun_binds)
      }
      _ => {
        self.back(tok);
        let sd = self.str_dec()?;
        if let StrDec::Seq(ref s) = sd.val {
          if s.is_empty() {
            return Ok(None);
          }
        }
        TopDec::StrDec(sd)
      }
    };
    Ok(Some(loc.wrap(ret)))
  }

  fn str_exp(&mut self) -> Result<Located<StrExp<StrRef>>> {
    let tok = self.next();
    let loc = tok.loc;
    let mut ret = match tok.val {
      Token::Struct => {
        let ops = self.ops.clone();
        let dec = self.str_dec()?;
        self.eat(Token::End)?;
        self.ops = ops;
        StrExp::Struct(dec)
      }
      Token::Let => {
        let ops = self.ops.clone();
        let dec = self.str_dec()?;
        self.eat(Token::In)?;
        let exp = self.str_exp()?;
        self.eat(Token::End)?;
        self.ops = ops;
        StrExp::Let(dec.into(), exp.into())
      }
      Token::Ident(_, IdentType::AlphaNum) => {
        self.back(tok);
        let long_id = self.long_alpha_num_id()?;
        let tok = self.next();
        if let Token::LRound = tok.val {
          let exp = self.str_exp()?;
          self.eat(Token::RRound)?;
          StrExp::FunctorApp(long_id, exp.into())
        } else {
          StrExp::LongStrId(long_id)
        }
      }
      _ => return self.fail("a structure expression", tok),
    };
    loop {
      let tok = self.next();
      ret = match tok.val {
        Token::Colon => {
          let exp = self.sig_exp()?;
          StrExp::Transparent(loc.wrap(ret).into(), exp)
        }
        Token::ColonGt => {
          let exp = self.sig_exp()?;
          StrExp::Opaque(loc.wrap(ret).into(), exp)
        }
        _ => {
          self.back(tok);
          break;
        }
      };
    }
    Ok(loc.wrap(ret))
  }

  fn maybe_str_dec(&mut self) -> Result<Option<Located<StrDec<StrRef>>>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = match tok.val {
      Token::Structure => {
        let mut str_binds = Vec::new();
        loop {
          let id = self.alpha_num_id()?;
          self.eat(Token::Equal)?;
          let exp = self.str_exp()?;
          str_binds.push(StrBind { id, exp });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        StrDec::Structure(str_binds)
      }
      Token::Local => {
        let ops = self.ops.clone();
        let fst = self.str_dec()?;
        self.eat(Token::In)?;
        let snd = self.str_dec()?;
        self.eat(Token::End)?;
        self.ops = ops;
        StrDec::Local(fst.into(), snd.into())
      }
      _ => {
        self.back(tok);
        let dec = self.dec()?;
        if let Dec::Seq(ref xs) = dec.val {
          if xs.is_empty() {
            return Ok(None);
          }
        }
        StrDec::Dec(dec)
      }
    };
    Ok(Some(loc.wrap(ret)))
  }

  fn str_dec(&mut self) -> Result<Located<StrDec<StrRef>>> {
    self.semicolon_seq(Self::maybe_str_dec, StrDec::Seq)
  }

  fn sig_exp(&mut self) -> Result<Located<SigExp<StrRef>>> {
    let tok = self.next();
    let loc = tok.loc;
    let mut ret = match tok.val {
      Token::Sig => {
        let spec = self.spec()?;
        self.eat(Token::End)?;
        SigExp::Sig(spec)
      }
      Token::Ident(_, IdentType::AlphaNum) => {
        self.back(tok);
        let long_id = self.long_alpha_num_id()?;
        SigExp::SigId(long_id)
      }
      _ => return self.fail("a signature expression", tok),
    };
    loop {
      let tok = self.next();
      if let Token::Where = tok.val {
        let ty_vars = self.ty_var_seq()?;
        let ty_con = self.long_id(true)?;
        self.eat(Token::Equal)?;
        let ty = self.ty()?;
        ret = SigExp::Where(loc.wrap(ret).into(), ty_vars, ty_con, ty);
      } else {
        self.back(tok);
        break;
      }
    }
    Ok(loc.wrap(ret))
  }

  fn maybe_spec(&mut self) -> Result<Option<Located<Spec<StrRef>>>> {
    let tok = self.next();
    let loc = tok.loc;
    let mut ret = match tok.val {
      Token::Val => {
        let mut val_descs = Vec::new();
        loop {
          let vid = self.ident()?;
          self.eat(Token::Colon)?;
          let ty = self.ty()?;
          val_descs.push(ValDesc { vid, ty });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        Spec::Val(val_descs)
      }
      Token::Type => Spec::Type(self.ty_descs()?),
      Token::Eqtype => Spec::Eqtype(self.ty_descs()?),
      Token::Datatype => self.spec_datatype()?,
      Token::Exception => {
        let mut ex_descs = Vec::new();
        loop {
          let vid = self.ident()?;
          let ty = self.maybe_of_ty()?;
          ex_descs.push(ExDesc { vid, ty });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        Spec::Exception(ex_descs)
      }
      Token::Structure => {
        let mut str_descs = Vec::new();
        loop {
          let str_id = self.alpha_num_id()?;
          self.eat(Token::Colon)?;
          let exp = self.sig_exp()?;
          str_descs.push(StrDesc { str_id, exp });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        Spec::Structure(str_descs)
      }
      Token::Include => {
        let exp = self.sig_exp()?;
        Spec::Include(exp.into())
      }
      _ => {
        self.back(tok);
        return Ok(None);
      }
    };
    loop {
      let tok = self.next();
      if let Token::Sharing = tok.val {
        self.eat(Token::Type)?;
        let mut ty_cons = Vec::new();
        loop {
          ty_cons.push(self.long_id(true)?);
          let tok = self.next();
          if let Token::Equal = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        if ty_cons.len() < 2 {
          let tok = self.next();
          return self.fail("an identifier", tok);
        }
        ret = Spec::Sharing(ty_cons);
      } else {
        self.back(tok);
        break;
      }
    }
    Ok(Some(loc.wrap(ret)))
  }

  fn spec_datatype(&mut self) -> Result<Spec<StrRef>> {
    let tok = self.next();
    let dat_desc = if let Token::Ident(id, _) = tok.val {
      let ty_con = tok.loc.wrap(id);
      self.eat(Token::Equal)?;
      let tok = self.next();
      if let Token::Datatype = tok.val {
        let long = self.long_id(true)?;
        return Ok(Spec::DatatypeCopy(ty_con, long));
      }
      self.back(tok);
      let cons = self.con_descs()?;
      DatDesc {
        ty_vars: Vec::new(),
        ty_con,
        cons,
      }
    } else {
      self.back(tok);
      self.dat_desc()?
    };
    let mut dat_descs = vec![dat_desc];
    loop {
      let tok = self.next();
      if let Token::And = tok.val {
        dat_descs.push(self.dat_desc()?);
      } else {
        self.back(tok);
        break;
      }
    }
    Ok(Spec::Datatype(dat_descs))
  }

  fn dat_desc(&mut self) -> Result<DatDesc<StrRef>> {
    let ty_vars = self.ty_var_seq()?;
    let ty_con = self.ident()?;
    self.eat(Token::Equal)?;
    let cons = self.con_descs()?;
    Ok(DatDesc {
      ty_vars,
      ty_con,
      cons,
    })
  }

  fn con_descs(&mut self) -> Result<Vec<ConDesc<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      let vid = self.ident()?;
      let ty = self.maybe_of_ty()?;
      ret.push(ConDesc { vid, ty });
      let tok = self.next();
      if let Token::Bar = tok.val {
        continue;
      }
      self.back(tok);
      break;
    }
    Ok(ret)
  }

  fn ty_descs(&mut self) -> Result<Vec<TyDesc<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      let ty_vars = self.ty_var_seq()?;
      let ty_con = self.ident()?;
      ret.push(TyDesc { ty_vars, ty_con });
      let tok = self.next();
      if let Token::And = tok.val {
        continue;
      }
      self.back(tok);
      break;
    }
    Ok(ret)
  }

  fn spec(&mut self) -> Result<Located<Spec<StrRef>>> {
    self.semicolon_seq(Self::maybe_spec, Spec::Seq)
  }

  fn maybe_at_exp(&mut self) -> Result<Option<Located<Exp<StrRef>>>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = match tok.val {
      Token::DecInt(n, _) => Exp::DecInt(n),
      Token::HexInt(n) => Exp::HexInt(n),
      Token::DecWord(n) => Exp::DecWord(n),
      Token::HexWord(n) => Exp::HexWord(n),
      Token::Real(n) => Exp::Real(n),
      Token::Str(s) => Exp::Str(s),
      Token::Char(c) => Exp::Char(c),
      Token::Op => Exp::LongVid(self.long_id(true)?),
      Token::LCurly => {
        let tok = self.next();
        let mut rows = Vec::new();
        if let Token::RCurly = tok.val {
          //
        } else {
          self.back(tok);
          loop {
            let lab = self.label()?;
            self.eat(Token::Equal)?;
            let exp = self.exp()?;
            rows.push(Row { lab, exp });
            let tok = self.next();
            match tok.val {
              Token::RCurly => break,
              Token::Comma => continue,
              _ => return self.fail("`}` or `,`", tok),
            }
          }
        }
        Exp::Record(rows)
      }
      Token::Pound => Exp::Select(self.label()?),
      Token::LRound => {
        let tok = self.next();
        if let Token::RRound = tok.val {
          return Ok(Some(loc.wrap(Exp::Tuple(Vec::new()))));
        }
        self.back(tok);
        let fst = self.exp()?;
        let tok = self.next();
        match tok.val {
          Token::RRound => fst.val,
          Token::Comma => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let tok = self.next();
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
              let tok = self.next();
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
        let tok = self.next();
        let mut exprs = Vec::new();
        if let Token::RSquare = tok.val {
          //
        } else {
          self.back(tok);
          loop {
            exprs.push(self.exp()?);
            let tok = self.next();
            match tok.val {
              Token::RSquare => break,
              Token::Comma => continue,
              _ => return self.fail("`]` or `,`", tok),
            }
          }
        }
        Exp::List(exprs)
      }
      Token::Let => {
        let ops = self.ops.clone();
        let dec = self.dec()?;
        self.eat(Token::In)?;
        let mut exprs = Vec::new();
        loop {
          exprs.push(self.exp()?);
          let tok = self.next();
          match tok.val {
            Token::End => break,
            Token::Semicolon => continue,
            _ => return self.fail("`end` or `;`", tok),
          }
        }
        self.ops = ops;
        Exp::Let(dec, exprs)
      }
      Token::Ident(..) => {
        self.back(tok);
        Exp::LongVid(self.long_id(false)?)
      }
      _ => {
        // this is the one time we return Ok(None). we need this info to do
        // application expressions correctly.
        self.back(tok);
        return Ok(None);
      }
    };
    Ok(Some(loc.wrap(ret)))
  }

  fn at_exp(&mut self) -> Result<Located<Exp<StrRef>>> {
    match self.maybe_at_exp()? {
      Some(x) => Ok(x),
      None => {
        let tok = self.next();
        self.fail("an expression", tok)
      }
    }
  }

  fn ident(&mut self) -> Result<Located<StrRef>> {
    let tok = self.next();
    if let Token::Ident(id, _) = tok.val {
      Ok(tok.loc.wrap(id))
    } else {
      self.fail("an identifier", tok)
    }
  }

  fn alpha_num_id(&mut self) -> Result<Located<StrRef>> {
    let tok = self.next();
    if let Token::Ident(id, IdentType::AlphaNum) = tok.val {
      Ok(tok.loc.wrap(id))
    } else {
      self.fail("an identifier", tok)
    }
  }

  fn maybe_long_id(&mut self) -> Result<Option<Long<StrRef>>> {
    let mut idents = Vec::new();
    loop {
      let tok = self.next();
      if let Token::Ident(id, typ) = tok.val {
        idents.push(tok.loc.wrap(id));
        if let IdentType::Symbolic = typ {
          break;
        }
        let tok = self.next();
        if let Token::Dot = tok.val {
          continue;
        }
        self.back(tok);
        break;
      }
      return if idents.is_empty() {
        self.back(tok);
        Ok(None)
      } else {
        self.fail("an identifier", tok)
      };
    }
    let last = idents.pop().unwrap();
    Ok(Some(Long { idents, last }))
  }

  fn long_id(&mut self, allow_infix: bool) -> Result<Long<StrRef>> {
    let mut ret = match self.maybe_long_id()? {
      Some(x) => x,
      None => {
        let tok = self.next();
        return self.fail("an identifier", tok);
      }
    };
    if !allow_infix
      && ret.idents.len() == 1
      && self.ops.contains_key(&ret.idents.first().unwrap().val)
    {
      let id = ret.idents.pop().unwrap();
      eprintln!("NO {:?}", allow_infix);
      Err(id.loc.wrap(ParseError::InfixWithoutOp(id.val)))
    } else {
      Ok(ret)
    }
  }

  fn long_alpha_num_id(&mut self) -> Result<Long<StrRef>> {
    let mut idents = Vec::new();
    loop {
      let tok = self.next();
      if let Token::Ident(id, IdentType::AlphaNum) = tok.val {
        idents.push(tok.loc.wrap(id));
        let tok = self.next();
        if let Token::Dot = tok.val {
          continue;
        }
        self.back(tok);
        break;
      }
      return self.fail("an identifier", tok);
    }
    let last = idents.pop().unwrap();
    Ok(Long { idents, last })
  }

  fn label(&mut self) -> Result<Located<Label>> {
    let tok = self.next();
    let ret = match tok.val {
      Token::DecInt(n, IsNumLab::Maybe) => Label::Num(n.try_into().unwrap()),
      Token::Ident(id, _) => Label::Vid(id),
      _ => return self.fail("a label", tok),
    };
    Ok(tok.loc.wrap(ret))
  }

  // TODO prec
  fn exp(&mut self) -> Result<Located<Exp<StrRef>>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = match tok.val {
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
        loop {
          let tok = self.next();
          exp = exp.loc.wrap(match tok.val {
            Token::Ident(ref id, _) => match self.ops.get(id) {
              Some(op_info) => {
                let rhs = self.exp()?;
                Exp::InfixApp(exp.into(), tok.loc.wrap(*id), rhs.into())
              }
              None => {
                self.back(tok);
                let rhs = exp.loc.wrap(Exp::LongVid(self.long_id(true)?));
                Exp::App(exp.into(), rhs.into())
              }
            },
            Token::Colon => Exp::Typed(exp.into(), self.ty()?),
            Token::Andalso => Exp::Andalso(exp.into(), self.exp()?.into()),
            Token::Orelse => Exp::Orelse(exp.into(), self.exp()?.into()),
            Token::Handle => Exp::Handle(exp.into(), self.match_()?),
            _ => {
              self.back(tok);
              match self.maybe_at_exp()? {
                Some(rhs) => Exp::App(exp.into(), rhs.into()),
                None => break,
              }
            }
          });
        }
        exp.val
      }
    };
    Ok(loc.wrap(ret))
  }

  fn match_(&mut self) -> Result<Match<StrRef>> {
    let mut arms = Vec::new();
    loop {
      let pat = self.pat()?;
      self.eat(Token::BigArrow)?;
      let exp = self.exp()?;
      arms.push(Arm { pat, exp });
      let tok = self.next();
      if let Token::Bar = tok.val {
        continue;
      }
      self.back(tok);
      break;
    }
    Ok(Match { arms })
  }

  fn maybe_dec(&mut self) -> Result<Option<Located<Dec<StrRef>>>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = match tok.val {
      Token::Val => {
        let ty_vars = self.ty_var_seq()?;
        let mut val_binds = Vec::new();
        loop {
          let tok = self.next();
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
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        Dec::Val(ty_vars, val_binds)
      }
      Token::Fun => {
        // have to contort the case for seeing an initial `(` after the `fun` because it could be
        // the beginning of a ty var seq OR the wrapping of an infix fval bind case head.
        let tok = self.next();
        let (ty_vars, fst_case) = if let Token::LRound = tok.val {
          match self.maybe_at_pat()? {
            None => (self.ty_var_seq_inner()?, self.fval_bind_case()?),
            Some(fst) => {
              // copied from fval_bind_case
              let vid = self.ident()?;
              if !self.ops.contains_key(&vid.val) {
                return Err(vid.loc.wrap(ParseError::NotInfix(vid.val)));
              }
              let snd = self.at_pat()?;
              self.eat(Token::RRound)?;
              let pat = fst.loc.wrap(Pat::Tuple(vec![fst, snd]));
              (Vec::new(), self.fval_bind_case_inner(vid, pat)?)
            }
          }
        } else {
          self.back(tok);
          (self.ty_var_seq()?, self.fval_bind_case()?)
        };
        let mut cases = vec![fst_case];
        let mut binds = Vec::new();
        loop {
          let tok = self.next();
          if let Token::Bar = tok.val {
            cases.push(self.fval_bind_case()?);
            continue;
          }
          binds.push(FValBind { cases });
          if let Token::And = tok.val {
            cases = vec![self.fval_bind_case()?];
            continue;
          }
          self.back(tok);
          break;
        }
        Dec::Fun(ty_vars, binds)
      }
      Token::Type => Dec::Type(self.ty_binds()?),
      Token::Datatype => {
        let tok = self.next();
        let dat_bind = if let Token::Ident(id, _) = tok.val {
          let ty_con = tok.loc.wrap(id);
          self.eat(Token::Equal)?;
          let tok = self.next();
          if let Token::Datatype = tok.val {
            let long = self.long_id(true)?;
            return Ok(Some(loc.wrap(Dec::DatatypeCopy(ty_con, long))));
          }
          self.back(tok);
          let cons = self.con_binds()?;
          DatBind {
            ty_vars: Vec::new(),
            ty_con,
            cons,
          }
        } else {
          self.back(tok);
          self.dat_bind()?
        };
        let mut dat_binds = vec![dat_bind];
        loop {
          let tok = self.next();
          if let Token::And = tok.val {
            dat_binds.push(self.dat_bind()?);
          } else {
            self.back(tok);
            break;
          }
        }
        let tok = self.next();
        let ty_binds = if let Token::Withtype = tok.val {
          self.ty_binds()?
        } else {
          self.back(tok);
          Vec::new()
        };
        Dec::Datatype(dat_binds, ty_binds)
      }
      Token::Abstype => {
        let mut dat_binds = vec![self.dat_bind()?];
        loop {
          let tok = self.next();
          if let Token::And = tok.val {
            dat_binds.push(self.dat_bind()?);
          } else {
            self.back(tok);
            break;
          }
        }
        let tok = self.next();
        let ty_binds = if let Token::Withtype = tok.val {
          self.ty_binds()?
        } else {
          Vec::new()
        };
        self.eat(Token::With)?;
        let dec = self.dec()?;
        self.eat(Token::End)?;
        Dec::Abstype(dat_binds, ty_binds, dec.into())
      }
      Token::Exception => {
        let mut ex_binds = Vec::new();
        loop {
          self.maybe_op()?;
          let vid = self.ident()?;
          let tok = self.next();
          let inner = if let Token::Equal = tok.val {
            self.maybe_op()?;
            ExBindInner::Long(self.long_id(true)?)
          } else {
            self.back(tok);
            ExBindInner::Ty(self.maybe_of_ty()?)
          };
          ex_binds.push(ExBind { vid, inner });
          let tok = self.next();
          if let Token::And = tok.val {
            continue;
          }
          self.back(tok);
          break;
        }
        Dec::Exception(ex_binds)
      }
      Token::Local => {
        let ops = self.ops.clone();
        let fst = self.dec()?;
        self.eat(Token::In)?;
        let snd = self.dec()?;
        self.ops = ops;
        Dec::Local(fst.into(), snd.into())
      }
      Token::Open => {
        let mut str_ids = Vec::new();
        loop {
          str_ids.push(self.long_alpha_num_id()?);
          let tok = self.next();
          if let Token::Ident(..) = tok.val {
            self.back(tok);
            continue;
          }
          self.back(tok);
          break;
        }
        Dec::Open(str_ids)
      }
      Token::Infix => {
        let n = self.fixity_num()?;
        let idents = self.fixity_idents()?;
        for id in idents.iter() {
          self.ops.insert(id.val.clone(), OpInfo::left(n.val));
        }
        Dec::Infix(n, idents)
      }
      Token::Infixr => {
        let n = self.fixity_num()?;
        let idents = self.fixity_idents()?;
        for id in idents.iter() {
          self.ops.insert(id.val.clone(), OpInfo::right(n.val));
        }
        Dec::Infixr(n, idents)
      }
      Token::Nonfix => {
        let idents = self.fixity_idents()?;
        for id in idents.iter() {
          self.ops.remove(&id.val);
        }
        Dec::Nonfix(idents)
      }
      _ => {
        self.back(tok);
        return Ok(None);
      }
    };
    Ok(Some(loc.wrap(ret)))
  }

  fn dec(&mut self) -> Result<Located<Dec<StrRef>>> {
    self.semicolon_seq(Self::maybe_dec, Dec::Seq)
  }

  fn fval_bind_case_inner(
    &mut self,
    vid: Located<StrRef>,
    pat: Located<Pat<StrRef>>,
  ) -> Result<FValBindCase<StrRef>> {
    let mut pats = vec![pat];
    while let Some(pat) = self.maybe_at_pat()? {
      pats.push(pat);
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

  // NOTE this is not compliant with the spec (page 78): "the parentheses may also be dropped if `:
  // ty` or `=` follows immediately." I can't figure out a way to be both spec compliant and also
  // not require unbounded lookahead. also note there is already nastiness with `fun (`, see the Fun
  // case for maybe_dec.
  fn fval_bind_case(&mut self) -> Result<FValBindCase<StrRef>> {
    let tok = self.next();
    let (vid, pat) = match tok.val {
      Token::Op => (self.ident()?, self.at_pat()?),
      Token::LRound => {
        let fst = self.at_pat()?;
        let vid = self.ident()?;
        if !self.ops.contains_key(&vid.val) {
          return Err(vid.loc.wrap(ParseError::NotInfix(vid.val)));
        }
        let snd = self.at_pat()?;
        self.eat(Token::RRound)?;
        (vid, fst.loc.wrap(Pat::Tuple(vec![fst, snd])))
      }
      Token::Ident(vid, _) => {
        if self.ops.contains_key(&vid) {
          return Err(tok.loc.wrap(ParseError::InfixWithoutOp(vid)));
        }
        (tok.loc.wrap(vid), self.at_pat()?)
      }
      _ => return self.fail("`op`, `(`, or an identifier", tok),
    };
    self.fval_bind_case_inner(vid, pat)
  }

  fn ty_binds(&mut self) -> Result<Vec<TyBind<StrRef>>> {
    let mut ty_binds = Vec::new();
    loop {
      let ty_vars = self.ty_var_seq()?;
      let ty_con = self.ident()?;
      self.eat(Token::Equal)?;
      let ty = self.ty()?;
      ty_binds.push(TyBind {
        ty_vars,
        ty_con,
        ty,
      });
      let tok = self.next();
      if let Token::And = tok.val {
        continue;
      }
      self.back(tok);
      break;
    }
    Ok(ty_binds)
  }

  fn con_binds(&mut self) -> Result<Vec<ConBind<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      self.maybe_op()?;
      let vid = self.ident()?;
      let ty = self.maybe_of_ty()?;
      ret.push(ConBind { vid, ty });
      let tok = self.next();
      if let Token::Bar = tok.val {
        continue;
      }
      self.back(tok);
      break;
    }
    Ok(ret)
  }

  fn dat_bind(&mut self) -> Result<DatBind<StrRef>> {
    let ty_vars = self.ty_var_seq()?;
    let ty_con = self.ident()?;
    self.eat(Token::Equal)?;
    let cons = self.con_binds()?;
    Ok(DatBind {
      ty_vars,
      ty_con,
      cons,
    })
  }

  fn ty_var_seq_inner(&mut self) -> Result<Vec<Located<TyVar<StrRef>>>> {
    let mut ret = Vec::new();
    loop {
      let tok = self.next();
      if let Token::TyVar(ty_var) = tok.val {
        ret.push(tok.loc.wrap(ty_var));
      } else {
        return self.fail("a type variable", tok);
      }
      let tok = self.next();
      match tok.val {
        Token::RRound => break,
        Token::Comma => continue,
        _ => return self.fail("`)` or `,`", tok),
      }
    }
    Ok(ret)
  }

  fn ty_var_seq(&mut self) -> Result<Vec<Located<TyVar<StrRef>>>> {
    let tok = self.next();
    match tok.val {
      Token::TyVar(ty_var) => Ok(vec![tok.loc.wrap(ty_var)]),
      Token::LRound => self.ty_var_seq_inner(),
      _ => {
        self.back(tok);
        Ok(Vec::new())
      }
    }
  }

  fn maybe_at_pat(&mut self) -> Result<Option<Located<Pat<StrRef>>>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = match tok.val {
      Token::Underscore => Pat::Wildcard,
      Token::DecInt(n, _) => Pat::DecInt(n),
      Token::HexInt(n) => Pat::HexInt(n),
      Token::DecWord(n) => Pat::DecWord(n),
      Token::HexWord(n) => Pat::HexWord(n),
      Token::Real(..) => return Err(loc.wrap(ParseError::RealPat)),
      Token::Str(s) => Pat::Str(s),
      Token::Char(c) => Pat::Char(c),
      Token::Op => Pat::LongVid(self.long_id(true)?),
      Token::LCurly => {
        let tok = self.next();
        let mut rows = Vec::new();
        let mut rest_loc = None;
        if let Token::RCurly = tok.val {
          //
        } else {
          self.back(tok);
          loop {
            let tok = self.next();
            if let Token::DotDotDot = tok.val {
              rest_loc = Some(tok.loc);
              let tok = self.next();
              if let Token::RCurly = tok.val {
                break;
              }
              return self.fail("`}`", tok);
            }
            self.back(tok);
            let lab = self.label()?;
            let tok = self.next();
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
            let tok = self.next();
            match tok.val {
              Token::RCurly => break,
              Token::Comma => continue,
              _ => return self.fail("`}` or `,`", tok),
            }
          }
        }
        Pat::Record(rows, rest_loc)
      }
      Token::LRound => {
        let tok = self.next();
        let mut pats = Vec::new();
        if let Token::RRound = tok.val {
          //
        } else {
          self.back(tok);
          loop {
            pats.push(self.pat()?);
            let tok = self.next();
            match tok.val {
              Token::RRound => break,
              Token::Comma => continue,
              _ => return self.fail("`)` or `,`", tok),
            }
          }
        }
        if pats.len() == 1 {
          pats.pop().unwrap().val
        } else {
          Pat::Tuple(pats)
        }
      }
      Token::LSquare => {
        let tok = self.next();
        let mut pats = Vec::new();
        if let Token::RSquare = tok.val {
          //
        } else {
          self.back(tok);
          loop {
            pats.push(self.pat()?);
            let tok = self.next();
            match tok.val {
              Token::RSquare => break,
              Token::Comma => continue,
              _ => return self.fail("`]` or `,`", tok),
            }
          }
        }
        Pat::List(pats)
      }
      Token::Ident(..) => {
        self.back(tok);
        Pat::LongVid(self.long_id(false)?)
      }
      _ => {
        self.back(tok);
        return Ok(None);
      }
    };
    Ok(Some(loc.wrap(ret)))
  }

  fn at_pat(&mut self) -> Result<Located<Pat<StrRef>>> {
    match self.maybe_at_pat()? {
      Some(x) => Ok(x),
      None => {
        let tok = self.next();
        self.fail("a pattern", tok)
      }
    }
  }

  // TODO prec
  fn pat(&mut self) -> Result<Located<Pat<StrRef>>> {
    let mut ret = self.at_pat()?;
    if let Pat::LongVid(long_vid) = ret.val {
      ret = ret.loc.wrap(self.pat_long_vid(ret.loc, long_vid)?);
    }
    loop {
      let tok = self.next();
      ret = ret.loc.wrap(match tok.val {
        Token::Colon => {
          let ty = self.ty()?;
          Pat::Typed(ret.into(), ty)
        }
        Token::Ident(id, _) => {
          let rhs = self.pat()?;
          let op_info = match self.ops.get(&id) {
            Some(x) => x,
            None => return Err(tok.loc.wrap(ParseError::NotInfix(id))),
          };
          Pat::InfixCtor(ret.into(), tok.loc.wrap(id), rhs.into())
        }
        _ => {
          self.back(tok);
          break;
        }
      });
    }
    Ok(ret)
  }

  fn pat_long_vid(&mut self, loc: Loc, mut long_vid: Long<StrRef>) -> Result<Pat<StrRef>> {
    if long_vid.idents.len() == 1 {
      let ty = self.maybe_colon_ty()?;
      match self.maybe_as_pat()? {
        None => match ty {
          None => {}
          Some(ty) => return Ok(Pat::Typed(loc.wrap(Pat::LongVid(long_vid)).into(), ty)),
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

  fn maybe_colon_ty(&mut self) -> Result<Option<Located<Ty<StrRef>>>> {
    let tok = self.next();
    if let Token::Colon = tok.val {
      Ok(Some(self.ty()?))
    } else {
      self.back(tok);
      Ok(None)
    }
  }

  fn maybe_as_pat(&mut self) -> Result<Option<Located<Pat<StrRef>>>> {
    let tok = self.next();
    if let Token::As = tok.val {
      Ok(Some(self.pat()?))
    } else {
      self.back(tok);
      Ok(None)
    }
  }

  fn ty(&mut self) -> Result<Located<Ty<StrRef>>> {
    self.ty_prec(TyPrec::Arrow)
  }

  fn ty_prec(&mut self, min_prec: TyPrec) -> Result<Located<Ty<StrRef>>> {
    let tok = self.next();
    let loc = tok.loc;
    let mut ret = match tok.val {
      Token::TyVar(tv) => Ty::TyVar(tv),
      Token::LCurly => {
        let tok = self.next();
        let mut rows = Vec::new();
        if let Token::RCurly = tok.val {
          //
        } else {
          self.back(tok);
          loop {
            let lab = self.label()?;
            self.eat(Token::Colon)?;
            let ty = self.ty()?;
            rows.push(TyRow { lab, ty });
            let tok = self.next();
            match tok.val {
              Token::RCurly => break,
              Token::Comma => continue,
              _ => return self.fail("`}` or `,`", tok),
            }
          }
        }
        Ty::Record(rows)
      }
      Token::LRound => {
        let mut types = Vec::new();
        loop {
          types.push(self.ty()?);
          match tok.val {
            Token::RRound => break,
            Token::Comma => continue,
            _ => return self.fail("`(` or `,`", tok),
          }
        }
        let long_ty_con = self.maybe_long_id()?;
        match (types.len(), long_ty_con) {
          (1, None) => types.pop().unwrap().val,
          (_, None) => {
            let tok = self.next();
            return self.fail("an identifier", tok);
          }
          (_, Some(x)) => Ty::TyCon(types, x),
        }
      }
      Token::Ident(ref id, _) => {
        if *id == StrRef::STAR {
          return self.fail("an identifier", tok);
        }
        self.back(tok);
        let long_ty_con = self.long_id(true)?;
        Ty::TyCon(Vec::new(), long_ty_con)
      }
      _ => return self.fail("a type", tok),
    };
    loop {
      let tok = self.next();
      match tok.val {
        Token::Arrow => {
          if TyPrec::Arrow < min_prec {
            self.back(tok);
            break;
          }
          let rhs = self.ty_prec(TyPrec::Arrow)?;
          ret = Ty::Arrow(loc.wrap(ret).into(), rhs.into());
        }
        Token::Ident(ref id, _) => {
          if *id == StrRef::STAR {
            if TyPrec::Star < min_prec {
              self.back(tok);
              break;
            }
            let mut types = vec![loc.wrap(ret)];
            loop {
              types.push(self.ty_prec(TyPrec::App)?);
              let tok = self.next();
              if let Token::Ident(ref id, _) = tok.val {
                if *id == StrRef::STAR {
                  continue;
                }
              }
              self.back(tok);
              break;
            }
            ret = Ty::Tuple(types);
          } else {
            if TyPrec::App < min_prec {
              unreachable!()
            }
            self.back(tok);
            let long = self.long_id(true)?;
            ret = Ty::TyCon(vec![loc.wrap(ret)], long);
          }
        }
        _ => break,
      }
    }
    Ok(loc.wrap(ret))
  }

  fn semicolon_seq<T, F, G>(&mut self, one: F, seq: G) -> Result<Located<T>>
  where
    F: Fn(&mut Self) -> Result<Option<Located<T>>>,
    G: FnOnce(Vec<Located<T>>) -> T,
  {
    let mut xs = Vec::new();
    while let Some(x) = one(self)? {
      xs.push(x);
      let tok = self.next();
      if let Token::Semicolon = tok.val {
        continue;
      }
      self.back(tok);
    }
    let ret = match xs.len() {
      0 => {
        // NOTE we conjure up a 'fake' loc
        let tok = self.next();
        let loc = tok.loc;
        self.back(tok);
        loc.wrap(seq(Vec::new()))
      }
      1 => xs.pop().unwrap(),
      _ => xs.first().unwrap().loc.wrap(seq(xs)),
    };
    Ok(ret)
  }

  fn maybe_op(&mut self) -> Result<bool> {
    let tok = self.next();
    if let Token::Op = tok.val {
      Ok(true)
    } else {
      self.back(tok);
      Ok(false)
    }
  }

  fn maybe_of_ty(&mut self) -> Result<Option<Located<Ty<StrRef>>>> {
    let tok = self.next();
    if let Token::Of = tok.val {
      Ok(Some(self.ty()?))
    } else {
      self.back(tok);
      Ok(None)
    }
  }

  fn fixity_num(&mut self) -> Result<Located<u32>> {
    let tok = self.next();
    let loc = tok.loc;
    let ret = if let Token::DecInt(n, _) = tok.val {
      if n < 0 {
        return Err(loc.wrap(ParseError::NegativeFixity(n)));
      }
      n.try_into().unwrap()
    } else {
      self.back(tok);
      0
    };
    Ok(loc.wrap(ret))
  }

  fn fixity_idents(&mut self) -> Result<Vec<Located<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      let tok = self.next();
      if let Token::Ident(id, _) = tok.val {
        ret.push(tok.loc.wrap(id));
        continue;
      }
      self.back(tok);
      break;
    }
    Ok(ret)
  }
}

#[derive(Clone)]
struct OpInfo {
  num: u32,
  assoc: Assoc,
}

impl OpInfo {
  fn left(num: u32) -> Self {
    Self {
      num,
      assoc: Assoc::Left,
    }
  }

  fn right(num: u32) -> Self {
    Self {
      num,
      assoc: Assoc::Right,
    }
  }
}

#[derive(Clone)]
enum Assoc {
  Left,
  Right,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

#[test]
fn test_ty_prec() {
  assert!(TyPrec::Arrow < TyPrec::Star);
  assert!(TyPrec::Star < TyPrec::App);
}
