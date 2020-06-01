use crate::ast::{Dec, Exp, Label, Long, Match, Row, Ty};
use crate::ident::Ident;
use crate::lex::{LexError, Lexer};
use crate::source::{Loc, Located};
use crate::token::Token;
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
  lookahead: Option<(Loc, Token)>,
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

  fn next(&mut self) -> Result<(Loc, Token)> {
    if let Some(look) = self.lookahead.take() {
      return Ok(look);
    }
    let tok = match self.lex.next() {
      Ok(x) => x,
      Err(e) => return Err(e.loc.wrap(e.val.into())),
    };
    Ok((tok.loc, tok.val))
  }

  fn eat(&mut self, tok: Token) -> Result<()> {
    let (loc, got) = self.next()?;
    if got == tok {
      Ok(())
    } else {
      self.fail(tok.desc(), loc, got)
    }
  }

  fn back(&mut self, loc: Loc, tok: Token) {
    assert!(self.lookahead.is_none());
    self.lookahead = Some((loc, tok));
  }

  /// returns:
  /// - Ok(Some(..)) if did parse an atomic exp.
  /// - Ok(None) if couldn't parse an atomic exp and didn't consume tokens.
  /// - Err(..) if couldn't parse an atomic exp and did consume tokens.
  fn at_exp(&mut self) -> Result<Option<Located<Exp<Ident>>>> {
    let (loc, tok) = self.next()?;
    let exp = match tok {
      Token::MaybeNumLab(n) | Token::DecInt(n) => Exp::DecInt(n),
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
          let (loc2, tok2) = self.next()?;
          if let Token::RCurly = tok2 {
            break;
          }
          self.back(loc2, tok2);
          let lab = self.label()?;
          self.eat(Token::Equal)?;
          let exp = self.exp()?;
          rows.push(Row { lab, exp });
        }
        Exp::Record(rows)
      }
      Token::Pound => Exp::Select(self.label()?),
      Token::LRound => {
        let (loc2, tok2) = self.next()?;
        if let Token::RRound = tok2 {
          return Ok(Some(loc.wrap(Exp::Tuple(Vec::new()))));
        }
        self.back(loc2, tok2);
        let fst = self.exp()?;
        let (loc2, tok2) = self.next()?;
        match tok2 {
          Token::RRound => fst.val,
          Token::Comma => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let (loc2, tok2) = self.next()?;
              match tok2 {
                Token::RRound => break,
                Token::Comma => continue,
                _ => return self.fail("`)` or `,`", loc2, tok2),
              }
            }
            Exp::Tuple(exprs)
          }
          Token::Semicolon => {
            let mut exprs = vec![fst];
            loop {
              exprs.push(self.exp()?);
              let (loc2, tok2) = self.next()?;
              match tok2 {
                Token::RRound => break,
                Token::Semicolon => continue,
                _ => return self.fail("`)` or `;`", loc2, tok2),
              }
            }
            Exp::Sequence(exprs)
          }
          _ => return self.fail("`)`, `,`, or `;`", loc2, tok2),
        }
      }
      Token::LSquare => {
        let (loc2, tok2) = self.next()?;
        if let Token::RSquare = tok2 {
          return Ok(Some(loc.wrap(Exp::List(Vec::new()))));
        }
        self.back(loc2, tok2);
        let mut exprs = Vec::new();
        loop {
          exprs.push(self.exp()?);
          let (loc2, tok2) = self.next()?;
          match tok2 {
            Token::RSquare => break,
            Token::Comma => continue,
            _ => return self.fail("`]` or `,`", loc2, tok2),
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
          let (loc2, tok2) = self.next()?;
          match tok2 {
            Token::End => break,
            Token::Semicolon => continue,
            _ => return self.fail("`end` or `;`", loc2, tok2),
          }
        }
        Exp::Let(dec, exprs)
      }
      Token::AlphaNumId(ref id) | Token::SymbolicId(ref id) => {
        if self.ops.contains_key(id) {
          return Err(loc.wrap(ParseError::InfixWithoutOp(id.clone())));
        }
        self.back(loc, tok);
        Exp::LongVid(self.long_vid()?)
      }
      _ => {
        // this is the one time we return Ok(None). we need this info to do
        // application expressions correctly.
        self.back(loc, tok);
        return Ok(None);
      }
    };
    Ok(Some(loc.wrap(exp)))
  }

  fn long_vid(&mut self) -> Result<Long<Ident>> {
    let mut idents = Vec::new();
    loop {
      let (loc, tok) = self.next()?;
      match tok {
        Token::AlphaNumId(id) => {
          idents.push(loc.wrap(id));
          let (loc2, tok2) = self.next()?;
          if let Token::Dot = tok2 {
            continue;
          } else {
            self.back(loc2, tok2);
            return Ok(Long { idents });
          }
        }
        Token::SymbolicId(id) => {
          idents.push(loc.wrap(id));
          return Ok(Long { idents });
        }
        _ => return self.fail("an identifier", loc, tok),
      }
    }
  }

  fn label(&mut self) -> Result<Located<Label>> {
    let (loc, tok) = self.next()?;
    let lab = match tok {
      Token::MaybeNumLab(n) => Label::Num(n.try_into().unwrap()),
      Token::AlphaNumId(id) | Token::SymbolicId(id) => Label::Vid(id),
      _ => return self.fail("a label", loc, tok),
    };
    Ok(loc.wrap(lab))
  }

  // TODO prec
  fn exp(&mut self) -> Result<Located<Exp<Ident>>> {
    let (loc, tok) = self.next()?;
    let exp = match tok {
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
        self.back(loc, tok);
        let mut exp = match self.at_exp()? {
          Some(x) => x,
          None => {
            let (loc, tok) = self.next()?;
            return self.fail("an expression", loc, tok);
          }
        };
        while let Some(x) = self.at_exp()? {
          exp = exp.loc.wrap(Exp::App(exp.into(), x.into()));
        }
        loop {
          let (loc, tok) = self.next()?;
          exp = exp.loc.wrap(match tok {
            Token::AlphaNumId(id) | Token::SymbolicId(id) => {
              let op_info =
                self.ops.get(&id).expect("should have parsed as App");
              Exp::InfixApp(exp.into(), loc.wrap(id), self.exp()?.into())
            }
            Token::Colon => Exp::Typed(exp.into(), self.ty()?),
            Token::Andalso => Exp::Andalso(exp.into(), self.exp()?.into()),
            Token::Orelse => Exp::Orelse(exp.into(), self.exp()?.into()),
            Token::Handle => Exp::Handle(exp.into(), self.match_()?),
            _ => {
              self.back(loc, tok);
              break;
            }
          });
        }
        exp.val
      }
    };
    Ok(loc.wrap(exp))
  }

  fn dec(&mut self) -> Result<Located<Dec<Ident>>> {
    todo!()
  }

  fn match_(&mut self) -> Result<Match<Ident>> {
    todo!()
  }

  fn ty(&mut self) -> Result<Located<Ty<Ident>>> {
    todo!()
  }

  fn fail<T>(&mut self, exp: &'static str, loc: Loc, tok: Token) -> Result<T> {
    Err(loc.wrap(ParseError::ExpectedButFound(exp, tok.desc())))
  }
}
