use crate::ast::{Arm, Dec, Exp, Label, Long, Match, Pat, Row, Ty, ValBind};
use crate::ident::Ident;
use crate::lex::{LexError, Lexer};
use crate::source::Located;
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
    }
  }
}

impl std::error::Error for ParseError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::LexError(e) => Some(e),
      Self::ExpectedButFound(..) | Self::InfixWithoutOp(..) => None,
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

impl<'s> Parser<'s> {
  fn new(lex: Lexer<'s>) -> Self {
    Self {
      lex,
      lookahead: None,
      ops: HashMap::new(),
    }
  }

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

  fn eat(&mut self, tok: Token) -> Result<()> {
    let next = self.next()?;
    if next.val == tok {
      Ok(())
    } else {
      self.fail(tok.desc(), next)
    }
  }

  fn back(&mut self, tok: Located<Token>) {
    assert!(self.lookahead.is_none());
    self.lookahead = Some(tok);
  }

  /// returns:
  /// - Ok(Some(..)) if did parse an atomic exp.
  /// - Ok(None) if couldn't parse an atomic exp and didn't consume tokens.
  /// - Err(..) if couldn't parse an atomic exp and did consume tokens.
  fn at_exp(&mut self) -> Result<Option<Located<Exp<Ident>>>> {
    let tok = self.next()?;
    let loc = tok.loc;
    let exp = match tok.val {
      Token::DecInt(n, _) => Exp::DecInt(n),
      Token::HexInt(n) => Exp::HexInt(n),
      Token::DecWord(n) => Exp::DecWord(n),
      Token::HexWord(n) => Exp::HexWord(n),
      Token::Real(n) => Exp::Real(n),
      Token::Str(s) => Exp::Str(s),
      Token::Char(c) => Exp::Char(c),
      Token::Op => Exp::LongVid(self.long_vid()?),
      Token::LCurly => {
        let mut rows = Vec::new();
        loop {
          let tok2 = self.next()?;
          if let Token::RCurly = tok2.val {
            break;
          }
          self.back(tok2);
          let lab = self.label()?;
          self.eat(Token::Equal)?;
          let exp = self.exp()?;
          rows.push(Row { lab, exp });
        }
        Exp::Record(rows)
      }
      Token::Pound => Exp::Select(self.label()?),
      Token::LRound => {
        let tok2 = self.next()?;
        if let Token::RRound = tok2.val {
          return Ok(Some(tok.loc.wrap(Exp::Tuple(Vec::new()))));
        }
        self.back(tok2);
        let fst = self.exp()?;
        let tok2 = self.next()?;
        match tok2.val {
          Token::RRound => fst.val,
          Token::Comma => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let tok2 = self.next()?;
              match tok2.val {
                Token::RRound => break,
                Token::Comma => continue,
                _ => return self.fail("`)` or `,`", tok2),
              }
            }
            Exp::Tuple(exprs)
          }
          Token::Semicolon => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let tok2 = self.next()?;
              match tok2.val {
                Token::RRound => break,
                Token::Semicolon => continue,
                _ => return self.fail("`)` or `;`", tok2),
              }
            }
            Exp::Sequence(exprs)
          }
          _ => return self.fail("`)`, `,`, or `;`", tok2),
        }
      }
      Token::LSquare => {
        let tok2 = self.next()?;
        if let Token::RSquare = tok2.val {
          return Ok(Some(tok.loc.wrap(Exp::List(Vec::new()))));
        }
        self.back(tok2);
        let mut exprs = Vec::new();
        loop {
          exprs.push(self.exp()?);
          let tok2 = self.next()?;
          match tok2.val {
            Token::RSquare => break,
            Token::Comma => continue,
            _ => return self.fail("`]` or `,`", tok2),
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
          let tok2 = self.next()?;
          match tok2.val {
            Token::End => break,
            Token::Semicolon => continue,
            _ => return self.fail("`end` or `;`", tok2),
          }
        }
        Exp::Let(dec, exprs)
      }
      Token::Ident(ref id, _) => {
        if self.ops.contains_key(id) {
          return Err(tok.loc.wrap(ParseError::InfixWithoutOp(id.clone())));
        }
        self.back(tok);
        Exp::LongVid(self.long_vid()?)
      }
      _ => {
        // this is the one time we return Ok(None). we need this info to do
        // application expressions correctly.
        self.back(tok);
        return Ok(None);
      }
    };
    Ok(Some(loc.wrap(exp)))
  }

  fn long_vid(&mut self) -> Result<Long<Ident>> {
    let mut idents = Vec::new();
    loop {
      let tok = self.next()?;
      match tok.val {
        Token::Ident(id, typ) => {
          idents.push(tok.loc.wrap(id));
          if let IdentType::Symbolic = typ {
            return Ok(Long { idents });
          }
          let tok2 = self.next()?;
          if let Token::Dot = tok2.val {
            continue;
          } else {
            self.back(tok2);
            return Ok(Long { idents });
          }
        }
        _ => return self.fail("an identifier", tok),
      }
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
    let loc = tok.loc;
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
        let mut exp = match self.at_exp()? {
          Some(x) => x,
          None => {
            let tok = self.next()?;
            return self.fail("an expression", tok);
          }
        };
        while let Some(x) = self.at_exp()? {
          exp = exp.loc.wrap(Exp::App(exp.into(), x.into()));
        }
        loop {
          let tok = self.next()?;
          exp = exp.loc.wrap(match tok.val {
            Token::Ident(id, _) => {
              let op_info =
                self.ops.get(&id).expect("should have parsed as App");
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
    Ok(loc.wrap(exp))
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
      Token::Fun => todo!(),
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

  fn ty_var_seq(&mut self) -> Result<Vec<Located<TyVar<Ident>>>> {
    let tok = self.next()?;
    match tok.val {
      Token::TyVar(ty_var) => Ok(vec![tok.loc.wrap(ty_var)]),
      Token::LRound => {
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
      _ => {
        self.back(tok);
        Ok(Vec::new())
      }
    }
  }

  fn ty(&mut self) -> Result<Located<Ty<Ident>>> {
    todo!()
  }

  fn pat(&mut self) -> Result<Located<Pat<Ident>>> {
    todo!()
  }

  fn fail<T>(&mut self, exp: &'static str, tok: Located<Token>) -> Result<T> {
    let err = ParseError::ExpectedButFound(exp, tok.val.desc());
    Err(tok.loc.wrap(err))
  }
}
