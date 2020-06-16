//! Parsing.

use crate::ast::{
  Arm, Cases, ConBind, ConDesc, DatBind, DatDesc, Dec, ExBind, ExBindInner, ExDesc, Exp, FValBind,
  FValBindCase, FunBind, Label, Long, Pat, PatRow, Row, SigBind, SigExp, Spec, StrBind, StrDec,
  StrDesc, StrExp, TopDec, Ty, TyBind, TyDesc, TyRow, ValBind, ValDesc,
};
use crate::intern::StrRef;
use crate::lex::Lexer;
use crate::loc::{Loc, Located};
use crate::token::{IdentType, IsNumLab, Token, TyVar};
use maplit::hashmap;
use std::collections::HashMap;
use std::convert::TryInto as _;

pub type Result<T> = std::result::Result<T, Located<ParseError>>;

pub fn get(lexer: Lexer) -> Result<Vec<Located<TopDec<StrRef>>>> {
  let mut ret = Vec::new();
  let last_loc = match lexer.last_loc() {
    Some(x) => x,
    None => return Ok(ret),
  };
  let mut p = Parser::new(lexer, last_loc);
  loop {
    if let Token::EOF = p.peek().val {
      break;
    }
    ret.push(p.top_dec()?);
  }
  Ok(ret)
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
  i: usize,
  ops: HashMap<StrRef, OpInfo>,
  last_loc: Loc,
}

// note: the `maybe` family of functions return Result<Option<T>>. these functions return:
// - Ok(Some(..)) if they did parse a T
// - Ok(None) if they couldn't parse a T but didn't consume any tokens
// - Err(..) if they couldn't parse a T but did consume tokens

impl Parser {
  /// constructs a new Parser.
  fn new(lexer: Lexer, last_loc: Loc) -> Self {
    Self {
      lexer,
      last_loc,
      i: 0,
      ops: hashmap! {
        StrRef::CONS => OpInfo::right(5),
        StrRef::EQ => OpInfo::left(4),
        StrRef::ASSIGN => OpInfo::left(3),
        StrRef::DIV => OpInfo::left(7),
        StrRef::MOD => OpInfo::left(7),
        StrRef::STAR => OpInfo::left(7),
        StrRef::SLASH => OpInfo::left(7),
        StrRef::PLUS => OpInfo::left(6),
        StrRef::MINUS => OpInfo::left(6),
        StrRef::LT => OpInfo::left(4),
        StrRef::GT => OpInfo::left(4),
        StrRef::LT_EQ => OpInfo::left(4),
        StrRef::GT_EQ => OpInfo::left(4),
      },
    }
  }

  /// gets the current token. does not advance the parser.
  fn peek(&self) -> Located<Token> {
    match self.lexer.get(self.i) {
      Some(tok) => tok,
      None => self.last_loc.wrap(Token::EOF),
    }
  }

  /// advances the parser ahead 1 token.
  #[inline]
  fn skip(&mut self) {
    self.i += 1;
  }

  /// combines a 'begin' loc with the 'end' loc, which is the loc of the last token we consumed, and
  /// uses it to wrap val.
  fn wrap<T>(&self, begin: Loc, val: T) -> Located<T> {
    let end = match self.lexer.get(self.i - 1) {
      Some(tok) => tok.loc,
      None => self.last_loc,
    };
    begin.span(end).wrap(val)
  }

  /// if the current token is `tok`, return `Ok(())` and advance, else return `Err(..)`.
  fn eat(&mut self, tok: Token) -> Result<()> {
    let next = self.peek();
    if next.val == tok {
      self.skip();
      Ok(())
    } else {
      self.fail(tok.desc(), next)
    }
  }

  /// returns an ExpectedButFound error, where we expected `want` but got `tok`.
  fn fail<T>(&mut self, want: &'static str, tok: Located<Token>) -> Result<T> {
    let err = ParseError::ExpectedButFound(want, tok.val.desc());
    Err(tok.loc.wrap(err))
  }

  fn top_dec(&mut self) -> Result<Located<TopDec<StrRef>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let ret = match tok.val {
      Token::Signature => {
        self.skip();
        let mut sig_binds = Vec::new();
        loop {
          let id = self.alpha_num_id()?;
          self.eat(Token::Equal)?;
          let exp = self.sig_exp()?;
          sig_binds.push(SigBind { id, exp });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        TopDec::SigDec(sig_binds)
      }
      Token::Functor => {
        self.skip();
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
          if let Token::And = self.peek().val {
            continue;
          } else {
            break;
          }
        }
        TopDec::FunDec(fun_binds)
      }
      _ => {
        let sd = self.str_dec()?;
        if let StrDec::Seq(ref xs) = sd.val {
          if xs.is_empty() {
            return self.fail("a top-level declaration", self.peek());
          }
        }
        TopDec::StrDec(sd)
      }
    };
    Ok(self.wrap(begin, ret))
  }

  fn str_exp(&mut self) -> Result<Located<StrExp<StrRef>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let mut ret = match tok.val {
      Token::Struct => {
        self.skip();
        let ops = self.ops.clone();
        let dec = self.str_dec()?;
        self.eat(Token::End)?;
        self.ops = ops;
        StrExp::Struct(dec)
      }
      Token::Let => {
        self.skip();
        let ops = self.ops.clone();
        let dec = self.str_dec()?;
        self.eat(Token::In)?;
        let exp = self.str_exp()?;
        self.eat(Token::End)?;
        self.ops = ops;
        StrExp::Let(dec.into(), exp.into())
      }
      Token::Ident(_, IdentType::AlphaNum) => {
        let long_id = self.long_alpha_num_id()?;
        if let Token::LRound = self.peek().val {
          self.skip();
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
      ret = match self.peek().val {
        Token::Colon => {
          self.skip();
          let exp = self.sig_exp()?;
          StrExp::Transparent(self.wrap(begin, ret).into(), exp)
        }
        Token::ColonGt => {
          self.skip();
          let exp = self.sig_exp()?;
          StrExp::Opaque(self.wrap(begin, ret).into(), exp)
        }
        _ => break,
      };
    }
    Ok(self.wrap(begin, ret))
  }

  fn maybe_str_dec(&mut self) -> Result<Option<Located<StrDec<StrRef>>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let ret = match tok.val {
      Token::Structure => {
        self.skip();
        let mut str_binds = Vec::new();
        loop {
          let id = self.alpha_num_id()?;
          self.eat(Token::Equal)?;
          let exp = self.str_exp()?;
          str_binds.push(StrBind { id, exp });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        StrDec::Structure(str_binds)
      }
      Token::Local => {
        self.skip();
        let ops = self.ops.clone();
        let fst = self.str_dec()?;
        self.eat(Token::In)?;
        let snd = self.str_dec()?;
        self.eat(Token::End)?;
        self.ops = ops;
        StrDec::Local(fst.into(), snd.into())
      }
      _ => {
        let dec = self.dec()?;
        if let Dec::Seq(ref xs) = dec.val {
          if xs.is_empty() {
            return Ok(None);
          }
        }
        StrDec::Dec(dec)
      }
    };
    Ok(Some(self.wrap(begin, ret)))
  }

  fn str_dec(&mut self) -> Result<Located<StrDec<StrRef>>> {
    self.semicolon_seq(Self::maybe_str_dec, StrDec::Seq)
  }

  fn sig_exp(&mut self) -> Result<Located<SigExp<StrRef>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let mut ret = match tok.val {
      Token::Sig => {
        self.skip();
        let spec = self.spec()?;
        self.eat(Token::End)?;
        SigExp::Sig(spec)
      }
      Token::Ident(_, IdentType::AlphaNum) => {
        let long_id = self.long_alpha_num_id()?;
        SigExp::SigId(long_id)
      }
      _ => return self.fail("a signature expression", tok),
    };
    loop {
      if let Token::Where = self.peek().val {
        self.skip();
        let ty_vars = self.ty_var_seq()?;
        let ty_con = self.long_id(true)?;
        self.eat(Token::Equal)?;
        let ty = self.ty()?;
        ret = SigExp::Where(self.wrap(begin, ret).into(), ty_vars, ty_con, ty);
      } else {
        break;
      }
    }
    Ok(self.wrap(begin, ret))
  }

  fn maybe_spec(&mut self) -> Result<Option<Located<Spec<StrRef>>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let mut ret = match tok.val {
      Token::Val => {
        self.skip();
        let mut val_descs = Vec::new();
        loop {
          let vid = self.ident()?;
          self.eat(Token::Colon)?;
          let ty = self.ty()?;
          val_descs.push(ValDesc { vid, ty });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        Spec::Val(val_descs)
      }
      Token::Type => {
        self.skip();
        Spec::Type(self.ty_descs()?)
      }
      Token::Eqtype => {
        self.skip();
        Spec::Eqtype(self.ty_descs()?)
      }
      Token::Datatype => {
        self.skip();
        self.spec_datatype()?
      }
      Token::Exception => {
        self.skip();
        let mut ex_descs = Vec::new();
        loop {
          let vid = self.ident()?;
          let ty = self.maybe_of_ty()?;
          ex_descs.push(ExDesc { vid, ty });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        Spec::Exception(ex_descs)
      }
      Token::Structure => {
        self.skip();
        let mut str_descs = Vec::new();
        loop {
          let str_id = self.alpha_num_id()?;
          self.eat(Token::Colon)?;
          let exp = self.sig_exp()?;
          str_descs.push(StrDesc { str_id, exp });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        Spec::Structure(str_descs)
      }
      Token::Include => {
        self.skip();
        let exp = self.sig_exp()?;
        Spec::Include(exp.into())
      }
      _ => return Ok(None),
    };
    loop {
      if let Token::Sharing = self.peek().val {
        self.skip();
        self.eat(Token::Type)?;
        let mut ty_cons = Vec::new();
        loop {
          ty_cons.push(self.long_id(true)?);
          if let Token::Equal = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        if ty_cons.len() < 2 {
          return self.fail("an identifier", self.peek());
        }
        ret = Spec::Sharing(self.wrap(begin, ret).into(), ty_cons);
      } else {
        break;
      }
    }
    Ok(Some(self.wrap(begin, ret)))
  }

  fn spec_datatype(&mut self) -> Result<Spec<StrRef>> {
    let tok = self.peek();
    let dat_desc = if let Token::Ident(id, _) = tok.val {
      let ty_con = tok.loc.wrap(id);
      self.skip();
      self.eat(Token::Equal)?;
      if let Token::Datatype = self.peek().val {
        self.skip();
        let long = self.long_id(true)?;
        return Ok(Spec::DatatypeCopy(ty_con, long));
      }
      let cons = self.con_descs()?;
      DatDesc {
        ty_vars: Vec::new(),
        ty_con,
        cons,
      }
    } else {
      self.dat_desc()?
    };
    let mut dat_descs = vec![dat_desc];
    loop {
      if let Token::And = self.peek().val {
        self.skip();
        dat_descs.push(self.dat_desc()?);
      } else {
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
      if let Token::Bar = self.peek().val {
        self.skip();
      } else {
        break;
      }
    }
    Ok(ret)
  }

  fn ty_descs(&mut self) -> Result<Vec<TyDesc<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      let ty_vars = self.ty_var_seq()?;
      let ty_con = self.ident()?;
      ret.push(TyDesc { ty_vars, ty_con });
      if let Token::And = self.peek().val {
        self.skip();
      } else {
        break;
      }
    }
    Ok(ret)
  }

  fn spec(&mut self) -> Result<Located<Spec<StrRef>>> {
    self.semicolon_seq(Self::maybe_spec, Spec::Seq)
  }

  fn maybe_at_exp(&mut self) -> Result<Option<Located<Exp<StrRef>>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let ret = match tok.val {
      Token::DecInt(n, _) => {
        self.skip();
        Exp::DecInt(n)
      }
      Token::HexInt(n) => {
        self.skip();
        Exp::HexInt(n)
      }
      Token::DecWord(n) => {
        self.skip();
        Exp::DecWord(n)
      }
      Token::HexWord(n) => {
        self.skip();
        Exp::HexWord(n)
      }
      Token::Real(n) => {
        self.skip();
        Exp::Real(n)
      }
      Token::Str(s) => {
        self.skip();
        Exp::Str(s)
      }
      Token::Char(c) => {
        self.skip();
        Exp::Char(c)
      }
      Token::Op => {
        self.skip();
        Exp::LongVid(self.long_id(true)?)
      }
      Token::LCurly => {
        self.skip();
        let mut rows = Vec::new();
        if let Token::RCurly = self.peek().val {
          self.skip();
        } else {
          loop {
            let lab = self.label()?;
            self.eat(Token::Equal)?;
            let exp = self.exp()?;
            rows.push(Row { lab, exp });
            let tok = self.peek();
            self.skip();
            match tok.val {
              Token::RCurly => break,
              Token::Comma => continue,
              _ => return self.fail("`}` or `,`", tok),
            }
          }
        }
        Exp::Record(rows)
      }
      Token::Pound => {
        self.skip();
        Exp::Select(self.label()?)
      }
      Token::LRound => {
        self.skip();
        if let Token::RRound = self.peek().val {
          self.skip();
          return Ok(Some(self.wrap(begin, Exp::Tuple(Vec::new()))));
        }
        let fst = self.exp()?;
        let tok = self.peek();
        self.skip();
        match tok.val {
          Token::RRound => fst.val,
          Token::Comma => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let tok = self.peek();
              self.skip();
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
              let tok = self.peek();
              self.skip();
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
        self.skip();
        let mut exprs = Vec::new();
        if let Token::RSquare = self.peek().val {
          self.skip();
        } else {
          loop {
            exprs.push(self.exp()?);
            let tok = self.peek();
            self.skip();
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
        self.skip();
        let ops = self.ops.clone();
        let dec = self.dec()?;
        self.eat(Token::In)?;
        let mut exprs = Vec::new();
        loop {
          exprs.push(self.exp()?);
          let tok = self.peek();
          self.skip();
          match tok.val {
            Token::End => break,
            Token::Semicolon => continue,
            _ => return self.fail("`end` or `;`", tok),
          }
        }
        self.ops = ops;
        Exp::Let(dec, exprs)
      }
      Token::Ident(..) | Token::Equal => Exp::LongVid(self.long_id(false)?),
      _ => return Ok(None),
    };
    Ok(Some(self.wrap(begin, ret)))
  }

  fn at_exp(&mut self) -> Result<Located<Exp<StrRef>>> {
    match self.maybe_at_exp()? {
      Some(x) => Ok(x),
      None => {
        let tok = self.peek();
        self.fail("an expression", tok)
      }
    }
  }

  fn ident(&mut self) -> Result<Located<StrRef>> {
    let tok = self.peek();
    self.skip();
    match tok.val {
      Token::Ident(id, _) => Ok(tok.loc.wrap(id)),
      Token::Equal => Ok(tok.loc.wrap(StrRef::EQ)),
      _ => self.fail("an identifier", tok),
    }
  }

  fn alpha_num_id(&mut self) -> Result<Located<StrRef>> {
    let tok = self.peek();
    if let Token::Ident(id, IdentType::AlphaNum) = tok.val {
      self.skip();
      Ok(tok.loc.wrap(id))
    } else {
      self.fail("an identifier", tok)
    }
  }

  fn maybe_long_id(&mut self) -> Result<Option<Long<StrRef>>> {
    let mut structures = Vec::new();
    loop {
      let tok = self.peek();
      match tok.val {
        Token::Ident(id, typ) => {
          self.skip();
          structures.push(tok.loc.wrap(id));
          if let IdentType::Symbolic = typ {
            break;
          }
          if let Token::Dot = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        Token::Equal => {
          self.skip();
          structures.push(tok.loc.wrap(StrRef::EQ));
          break;
        }
        _ => {}
      }
      return if structures.is_empty() {
        Ok(None)
      } else {
        self.fail("an identifier", self.peek())
      };
    }
    let last = structures.pop().expect("empty structures list");
    Ok(Some(Long { structures, last }))
  }

  fn long_id(&mut self, allow_infix: bool) -> Result<Long<StrRef>> {
    let ret = match self.maybe_long_id()? {
      Some(x) => x,
      None => return self.fail("an identifier", self.peek()),
    };
    if !allow_infix && ret.structures.is_empty() && self.ops.contains_key(&ret.last.val) {
      Err(ret.last.loc.wrap(ParseError::InfixWithoutOp(ret.last.val)))
    } else {
      Ok(ret)
    }
  }

  fn long_alpha_num_id(&mut self) -> Result<Long<StrRef>> {
    let mut structures = Vec::new();
    loop {
      let tok = self.peek();
      if let Token::Ident(id, IdentType::AlphaNum) = tok.val {
        self.skip();
        structures.push(tok.loc.wrap(id));
        if let Token::Dot = self.peek().val {
          self.skip();
        } else {
          break;
        }
      }
      return self.fail("an identifier", self.peek());
    }
    let last = structures.pop().expect("empty structures list");
    Ok(Long { structures, last })
  }

  fn label(&mut self) -> Result<Located<Label>> {
    let tok = self.peek();
    self.skip();
    let ret = match tok.val {
      Token::DecInt(n, IsNumLab::Maybe) => {
        Label::Num(n.try_into().expect("couldn't convert a number"))
      }
      Token::Ident(id, _) => Label::Vid(id),
      _ => return self.fail("a label", tok),
    };
    Ok(tok.loc.wrap(ret))
  }

  fn exp(&mut self) -> Result<Located<Exp<StrRef>>> {
    self.exp_prec(None)
  }

  fn exp_prec(&mut self, min_prec: Option<u32>) -> Result<Located<Exp<StrRef>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let ret = match tok.val {
      Token::Raise => {
        self.skip();
        let e = self.exp()?;
        Exp::Raise(e.into())
      }
      Token::If => {
        self.skip();
        let e_cond = self.exp()?;
        self.eat(Token::Then)?;
        let e_then = self.exp()?;
        self.eat(Token::Else)?;
        let e_else = self.exp()?;
        Exp::If(e_cond.into(), e_then.into(), e_else.into())
      }
      Token::While => {
        self.skip();
        let e_cond = self.exp()?;
        self.eat(Token::Do)?;
        let e_body = self.exp()?;
        Exp::While(e_cond.into(), e_body.into())
      }
      Token::Case => {
        self.skip();
        let e_head = self.exp()?;
        self.eat(Token::Of)?;
        let cases = self.cases()?;
        Exp::Case(e_head.into(), cases)
      }
      Token::Fn => {
        self.skip();
        let cases = self.cases()?;
        Exp::Fn(cases)
      }
      _ => {
        let mut exp = self.at_exp()?;
        loop {
          let tok = self.peek();
          exp = exp.loc.wrap(match tok.val {
            Token::Ident(..) | Token::Equal => {
              let long = self.long_id(true)?;
              if long.structures.is_empty() {
                match self.ops.get(&long.last.val) {
                  Some(&op_info) => {
                    if Some(op_info.num) < min_prec {
                      break;
                    }
                    let rhs = self.exp_prec(Some(op_info.min_prec()))?;
                    Exp::InfixApp(exp.into(), tok.loc.wrap(long.last.val), rhs.into())
                  }
                  None => {
                    let rhs = exp.loc.wrap(Exp::LongVid(long));
                    Exp::App(exp.into(), rhs.into())
                  }
                }
              } else {
                let rhs = exp.loc.wrap(Exp::LongVid(long));
                Exp::App(exp.into(), rhs.into())
              }
            }
            Token::Colon => {
              if min_prec.is_some() {
                break;
              }
              self.skip();
              let ty = self.ty()?;
              Exp::Typed(exp.into(), ty)
            }
            Token::Andalso => {
              if min_prec.is_some() {
                break;
              }
              self.skip();
              let rhs = self.exp()?;
              Exp::Andalso(exp.into(), rhs.into())
            }
            Token::Orelse => {
              if min_prec.is_some() {
                break;
              }
              self.skip();
              let rhs = self.exp()?;
              Exp::Orelse(exp.into(), rhs.into())
            }
            Token::Handle => {
              if min_prec.is_some() {
                break;
              }
              self.skip();
              Exp::Handle(exp.into(), self.cases()?)
            }
            _ => match self.maybe_at_exp()? {
              Some(rhs) => Exp::App(exp.into(), rhs.into()),
              None => break,
            },
          });
        }
        exp.val
      }
    };
    Ok(self.wrap(begin, ret))
  }

  fn cases(&mut self) -> Result<Cases<StrRef>> {
    let mut arms = Vec::new();
    loop {
      let pat = self.pat()?;
      self.eat(Token::BigArrow)?;
      let exp = self.exp()?;
      arms.push(Arm { pat, exp });
      if let Token::Bar = self.peek().val {
        self.skip();
      } else {
        break;
      }
    }
    Ok(Cases { arms })
  }

  fn maybe_dec(&mut self) -> Result<Option<Located<Dec<StrRef>>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let ret = match tok.val {
      Token::Val => {
        self.skip();
        let ty_vars = self.ty_var_seq()?;
        let mut val_binds = Vec::new();
        loop {
          let rec = if let Token::Rec = self.peek().val {
            self.skip();
            true
          } else {
            false
          };
          let pat = self.pat()?;
          self.eat(Token::Equal)?;
          let exp = self.exp()?;
          val_binds.push(ValBind { rec, pat, exp });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        Dec::Val(ty_vars, val_binds)
      }
      Token::Fun => {
        // have to contort the case for seeing an initial `(` after the `fun` because it could be
        // the beginning of a ty var seq OR the wrapping of an infix fval bind case head.
        self.skip();
        let tok = self.peek();
        let (ty_vars, fst_case) = if let Token::LRound = tok.val {
          self.skip();
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
          (self.ty_var_seq()?, self.fval_bind_case()?)
        };
        let mut cases = vec![fst_case];
        let mut binds = Vec::new();
        loop {
          let tok = self.peek();
          if let Token::Bar = tok.val {
            self.skip();
            cases.push(self.fval_bind_case()?);
            continue;
          }
          binds.push(FValBind { cases });
          if let Token::And = tok.val {
            self.skip();
            cases = vec![self.fval_bind_case()?];
            continue;
          }
          break;
        }
        Dec::Fun(ty_vars, binds)
      }
      Token::Type => {
        self.skip();
        Dec::Type(self.ty_binds()?)
      }
      Token::Datatype => {
        self.skip();
        let tok = self.peek();
        let dat_bind = if let Token::Ident(id, _) = tok.val {
          self.skip();
          let ty_con = tok.loc.wrap(id);
          self.eat(Token::Equal)?;
          if let Token::Datatype = self.peek().val {
            self.skip();
            let long = self.long_id(true)?;
            return Ok(Some(self.wrap(begin, Dec::DatatypeCopy(ty_con, long))));
          }
          let cons = self.con_binds()?;
          DatBind {
            ty_vars: Vec::new(),
            ty_con,
            cons,
          }
        } else {
          self.dat_bind()?
        };
        let mut dat_binds = vec![dat_bind];
        loop {
          if let Token::And = self.peek().val {
            self.skip();
            dat_binds.push(self.dat_bind()?);
          } else {
            break;
          }
        }
        let ty_binds = if let Token::Withtype = self.peek().val {
          self.skip();
          self.ty_binds()?
        } else {
          Vec::new()
        };
        Dec::Datatype(dat_binds, ty_binds)
      }
      Token::Abstype => {
        self.skip();
        let mut dat_binds = vec![self.dat_bind()?];
        loop {
          if let Token::And = self.peek().val {
            self.skip();
            dat_binds.push(self.dat_bind()?);
          } else {
            break;
          }
        }
        let ty_binds = if let Token::Withtype = self.peek().val {
          self.skip();
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
        self.skip();
        let mut ex_binds = Vec::new();
        loop {
          if let Token::Op = self.peek().val {
            self.skip();
          }
          let vid = self.ident()?;
          let inner = if let Token::Equal = self.peek().val {
            self.skip();
            if let Token::Op = self.peek().val {
              self.skip();
            }
            ExBindInner::Long(self.long_id(true)?)
          } else {
            ExBindInner::Ty(self.maybe_of_ty()?)
          };
          ex_binds.push(ExBind { vid, inner });
          if let Token::And = self.peek().val {
            self.skip();
          } else {
            break;
          }
        }
        Dec::Exception(ex_binds)
      }
      Token::Local => {
        self.skip();
        let ops = self.ops.clone();
        let fst = self.dec()?;
        self.eat(Token::In)?;
        let snd = self.dec()?;
        self.ops = ops;
        Dec::Local(fst.into(), snd.into())
      }
      Token::Open => {
        self.skip();
        let mut str_ids = Vec::new();
        loop {
          str_ids.push(self.long_alpha_num_id()?);
          if let Token::Ident(..) = self.peek().val {
            continue;
          } else {
            break;
          }
        }
        Dec::Open(str_ids)
      }
      Token::Infix => {
        self.skip();
        let n = self.fixity_num()?;
        let idents = self.fixity_idents()?;
        for id in idents.iter() {
          self.ops.insert(id.val.clone(), OpInfo::left(n.val));
        }
        Dec::Infix(n, idents)
      }
      Token::Infixr => {
        self.skip();
        let n = self.fixity_num()?;
        let idents = self.fixity_idents()?;
        for id in idents.iter() {
          self.ops.insert(id.val.clone(), OpInfo::right(n.val));
        }
        Dec::Infixr(n, idents)
      }
      Token::Nonfix => {
        self.skip();
        let idents = self.fixity_idents()?;
        for id in idents.iter() {
          self.ops.remove(&id.val);
        }
        Dec::Nonfix(idents)
      }
      _ => return Ok(None),
    };
    Ok(Some(self.wrap(begin, ret)))
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
    let tok = self.peek();
    self.skip();
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
      if let Token::And = self.peek().val {
        self.skip();
      } else {
        break;
      }
    }
    Ok(ty_binds)
  }

  fn con_binds(&mut self) -> Result<Vec<ConBind<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      if let Token::Op = self.peek().val {
        self.skip();
      }
      let vid = self.ident()?;
      let ty = self.maybe_of_ty()?;
      ret.push(ConBind { vid, ty });
      if let Token::Bar = self.peek().val {
        self.skip();
      } else {
        break;
      }
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
      let tok = self.peek();
      if let Token::TyVar(ty_var) = tok.val {
        self.skip();
        ret.push(tok.loc.wrap(ty_var));
      } else {
        return self.fail("a type variable", tok);
      }
      let tok = self.peek();
      self.skip();
      match tok.val {
        Token::RRound => break,
        Token::Comma => continue,
        _ => return self.fail("`)` or `,`", tok),
      }
    }
    Ok(ret)
  }

  fn ty_var_seq(&mut self) -> Result<Vec<Located<TyVar<StrRef>>>> {
    let tok = self.peek();
    match tok.val {
      Token::TyVar(ty_var) => {
        self.skip();
        Ok(vec![tok.loc.wrap(ty_var)])
      }
      Token::LRound => {
        self.skip();
        self.ty_var_seq_inner()
      }
      _ => Ok(Vec::new()),
    }
  }

  fn maybe_at_pat(&mut self) -> Result<Option<Located<Pat<StrRef>>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let ret = match tok.val {
      Token::Underscore => {
        self.skip();
        Pat::Wildcard
      }
      Token::DecInt(n, _) => {
        self.skip();
        Pat::DecInt(n)
      }
      Token::HexInt(n) => {
        self.skip();
        Pat::HexInt(n)
      }
      Token::DecWord(n) => {
        self.skip();
        Pat::DecWord(n)
      }
      Token::HexWord(n) => {
        self.skip();
        Pat::HexWord(n)
      }
      Token::Real(..) => return Err(begin.wrap(ParseError::RealPat)),
      Token::Str(s) => {
        self.skip();
        Pat::Str(s)
      }
      Token::Char(c) => {
        self.skip();
        Pat::Char(c)
      }
      Token::Op => {
        self.skip();
        Pat::LongVid(self.long_id(true)?)
      }
      Token::LCurly => {
        self.skip();
        let mut rows = Vec::new();
        let mut rest_loc = None;
        if let Token::RCurly = self.peek().val {
          self.skip();
        } else {
          loop {
            let tok = self.peek();
            if let Token::DotDotDot = tok.val {
              self.skip();
              rest_loc = Some(tok.loc);
              if let Token::RCurly = self.peek().val {
                self.skip();
                break;
              }
              return self.fail("`}`", tok);
            }
            let lab = self.label()?;
            let tok = self.peek();
            let row = if let Token::Equal = tok.val {
              self.skip();
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
            let tok = self.peek();
            self.skip();
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
        self.skip();
        let tok = self.peek();
        let mut pats = Vec::new();
        if let Token::RRound = tok.val {
          self.skip();
        } else {
          loop {
            pats.push(self.pat()?);
            let tok = self.peek();
            self.skip();
            match tok.val {
              Token::RRound => break,
              Token::Comma => continue,
              _ => return self.fail("`)` or `,`", tok),
            }
          }
        }
        if pats.len() == 1 {
          pats.pop().expect("empty patterns list").val
        } else {
          Pat::Tuple(pats)
        }
      }
      Token::LSquare => {
        self.skip();
        let mut pats = Vec::new();
        if let Token::RSquare = self.peek().val {
          self.skip();
        } else {
          loop {
            pats.push(self.pat()?);
            let tok = self.peek();
            self.skip();
            match tok.val {
              Token::RSquare => break,
              Token::Comma => continue,
              _ => return self.fail("`]` or `,`", tok),
            }
          }
        }
        Pat::List(pats)
      }
      Token::Ident(..) => Pat::LongVid(self.long_id(false)?),
      _ => return Ok(None),
    };
    Ok(Some(self.wrap(begin, ret)))
  }

  fn at_pat(&mut self) -> Result<Located<Pat<StrRef>>> {
    match self.maybe_at_pat()? {
      Some(x) => Ok(x),
      None => self.fail("a pattern", self.peek()),
    }
  }

  fn pat(&mut self) -> Result<Located<Pat<StrRef>>> {
    self.pat_prec(None)
  }

  fn pat_prec(&mut self, min_prec: Option<u32>) -> Result<Located<Pat<StrRef>>> {
    let mut ret = self.at_pat()?;
    if let Pat::LongVid(long_vid) = ret.val {
      ret = ret.loc.wrap(self.pat_long_vid(ret.loc, long_vid)?);
    }
    loop {
      let tok = self.peek();
      ret = ret.loc.wrap(match tok.val {
        Token::Colon => {
          if min_prec.is_some() {
            break;
          }
          self.skip();
          let ty = self.ty()?;
          Pat::Typed(ret.into(), ty)
        }
        Token::Ident(id, _) => {
          let op_info = match self.ops.get(&id) {
            Some(x) => *x,
            None => return Err(tok.loc.wrap(ParseError::NotInfix(id))),
          };
          if Some(op_info.num) < min_prec {
            break;
          }
          self.skip();
          let rhs = self.pat_prec(Some(op_info.min_prec()))?;
          Pat::InfixCtor(ret.into(), tok.loc.wrap(id), rhs.into())
        }
        _ => break,
      });
    }
    Ok(ret)
  }

  fn pat_long_vid(&mut self, loc: Loc, long_vid: Long<StrRef>) -> Result<Pat<StrRef>> {
    if long_vid.structures.is_empty() {
      let ty = self.maybe_colon_ty()?;
      match self.maybe_as_pat()? {
        None => match ty {
          None => {}
          Some(ty) => return Ok(Pat::Typed(loc.wrap(Pat::LongVid(long_vid)).into(), ty)),
        },
        Some(as_pat) => {
          return Ok(Pat::As(long_vid.last, ty, as_pat.into()));
        }
      }
    }
    if let Token::Ident(id, _) = self.peek().val {
      if self.ops.get(&id).is_some() {
        // fall back out to pat_prec.
        return Ok(Pat::LongVid(long_vid));
      }
    }
    match self.maybe_at_pat()? {
      None => Ok(Pat::LongVid(long_vid)),
      Some(x) => Ok(Pat::Ctor(long_vid, x.into())),
    }
  }

  fn maybe_colon_ty(&mut self) -> Result<Option<Located<Ty<StrRef>>>> {
    if let Token::Colon = self.peek().val {
      self.skip();
      Ok(Some(self.ty()?))
    } else {
      Ok(None)
    }
  }

  fn maybe_as_pat(&mut self) -> Result<Option<Located<Pat<StrRef>>>> {
    if let Token::As = self.peek().val {
      self.skip();
      Ok(Some(self.pat()?))
    } else {
      Ok(None)
    }
  }

  fn ty(&mut self) -> Result<Located<Ty<StrRef>>> {
    self.ty_prec(TyPrec::Arrow)
  }

  fn ty_prec(&mut self, min_prec: TyPrec) -> Result<Located<Ty<StrRef>>> {
    let tok = self.peek();
    let begin = tok.loc;
    let mut ret = match tok.val {
      Token::TyVar(tv) => {
        self.skip();
        Ty::TyVar(tv)
      }
      Token::LCurly => {
        self.skip();
        let mut rows = Vec::new();
        if let Token::RCurly = self.peek().val {
          self.skip();
        } else {
          loop {
            let lab = self.label()?;
            self.eat(Token::Colon)?;
            let ty = self.ty()?;
            rows.push(TyRow { lab, ty });
            let tok = self.peek();
            self.skip();
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
        self.skip();
        let mut types = Vec::new();
        loop {
          types.push(self.ty()?);
          let tok = self.peek();
          self.skip();
          match tok.val {
            Token::RRound => break,
            Token::Comma => continue,
            _ => return self.fail("`(` or `,`", tok),
          }
        }
        let long_ty_con = self.maybe_long_id()?;
        match (types.len(), long_ty_con) {
          (1, None) => types.pop().expect("empty types list").val,
          (_, None) => return self.fail("an identifier", self.peek()),
          (_, Some(x)) => Ty::TyCon(types, x),
        }
      }
      Token::Ident(ref id, _) => {
        if *id == StrRef::STAR {
          return self.fail("an identifier", tok);
        }
        let long_ty_con = self.long_id(true)?;
        Ty::TyCon(Vec::new(), long_ty_con)
      }
      _ => return self.fail("a type", tok),
    };
    loop {
      let tok = self.peek();
      match tok.val {
        Token::Arrow => {
          if TyPrec::Arrow < min_prec {
            break;
          }
          let lhs = self.wrap(begin, ret);
          self.skip();
          let rhs = self.ty_prec(TyPrec::Arrow)?;
          ret = Ty::Arrow(lhs.into(), rhs.into());
        }
        Token::Ident(ref id, _) => {
          if *id == StrRef::STAR {
            if TyPrec::Star < min_prec {
              break;
            }
            let mut types = vec![self.wrap(begin, ret)];
            self.skip();
            loop {
              types.push(self.ty_prec(TyPrec::App)?);
              if let Token::Ident(ref id, _) = self.peek().val {
                if *id == StrRef::STAR {
                  self.skip();
                  continue;
                }
              }
              break;
            }
            ret = Ty::Tuple(types);
          } else if TyPrec::App < min_prec {
            unreachable!()
          } else {
            let lhs = self.wrap(begin, ret);
            let long = self.long_id(true)?;
            ret = Ty::TyCon(vec![lhs], long);
          }
        }
        _ => break,
      }
    }
    Ok(self.wrap(begin, ret))
  }

  fn semicolon_seq<T, F, G>(&mut self, one: F, seq: G) -> Result<Located<T>>
  where
    F: Fn(&mut Self) -> Result<Option<Located<T>>>,
    G: FnOnce(Vec<Located<T>>) -> T,
  {
    let mut xs = Vec::new();
    while let Some(x) = one(self)? {
      xs.push(x);
      if let Token::Semicolon = self.peek().val {
        self.skip();
      }
    }
    let ret = match xs.len() {
      // NOTE we conjure up a 'fake' loc in the 0 case
      0 => self.peek().loc.wrap(seq(Vec::new())),
      1 => xs.pop().expect("empty list"),
      _ => xs.first().expect("empty list").loc.wrap(seq(xs)),
    };
    Ok(ret)
  }

  fn maybe_of_ty(&mut self) -> Result<Option<Located<Ty<StrRef>>>> {
    if let Token::Of = self.peek().val {
      self.skip();
      Ok(Some(self.ty()?))
    } else {
      Ok(None)
    }
  }

  fn fixity_num(&mut self) -> Result<Located<u32>> {
    let tok = self.peek();
    let loc = tok.loc;
    let ret = if let Token::DecInt(n, _) = tok.val {
      if n < 0 {
        return Err(loc.wrap(ParseError::NegativeFixity(n)));
      }
      self.skip();
      n.try_into().expect("couldn't convert number")
    } else {
      0
    };
    Ok(loc.wrap(ret))
  }

  fn fixity_idents(&mut self) -> Result<Vec<Located<StrRef>>> {
    let mut ret = Vec::new();
    loop {
      let tok = self.peek();
      match tok.val {
        Token::Ident(id, _) => {
          self.skip();
          ret.push(tok.loc.wrap(id));
        }
        Token::Equal => {
          self.skip();
          ret.push(tok.loc.wrap(StrRef::EQ));
        }
        _ => break,
      }
    }
    if ret.is_empty() {
      self.fail("an identifier", self.peek())
    } else {
      Ok(ret)
    }
  }
}

#[derive(Clone, Copy)]
struct OpInfo {
  num: u32,
  assoc: Assoc,
}

impl OpInfo {
  /// Returns a new OpInfo with left associativity.
  fn left(num: u32) -> Self {
    // we won't ever sub 1 from this in min_prec, but add 1 anyway to be consistent with `right`.
    Self {
      num: num + 1,
      assoc: Assoc::Left,
    }
  }

  /// Returns a new OpInfo with right associativity.
  fn right(num: u32) -> Self {
    // add one to the num so we can maybe later sub 1 in min_prec if num == 0.
    Self {
      num: num + 1,
      assoc: Assoc::Right,
    }
  }

  /// Calculates the minimum precedence for this OpInfo. This is suitable to be passed to a
  /// recursive call to a Pratt parser function.
  fn min_prec(&self) -> u32 {
    match self.assoc {
      Assoc::Left => self.num,
      Assoc::Right => self.num - 1,
    }
  }
}

#[derive(Clone, Copy)]
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

#[test]
fn option_compare() {
  let none: Option<usize> = None;
  assert!(none == none);
  assert!(none < Some(3));
  assert!(Some(3) == Some(3));
  assert!(Some(3) < Some(5));
}
