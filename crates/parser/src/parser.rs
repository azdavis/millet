pub(crate) mod marker;

mod parse_error;
pub(crate) use parse_error::ParseError;

use crate::event::Event;
use crate::grammar::{self, OpInfo};
use crate::source::Source;
use lexer::{Token, TokenKind};
use maplit::hashmap;
use marker::Marker;
use syntax::SyntaxKind;

use std::collections::HashMap;
use std::mem;

// TODO: probably add top level declaration intros to this (incl. modules)
const RECOVERY_SET: [TokenKind; 2] = [TokenKind::VAL, TokenKind::FUN];

#[derive(Debug)]
pub(crate) struct Parser<'t, 'input> {
  source: Source<'t, 'input>,
  events: Vec<Event>,
  expected_kinds: Vec<TokenKind>,
  ops: HashMap<&'input str, OpInfo>,
}

impl<'t, 'input> Parser<'t, 'input> {
  pub(crate) fn new(source: Source<'t, 'input>) -> Self {
    Self {
      source,
      events: Vec::new(),
      expected_kinds: Vec::new(),
      ops: hashmap! {
        "::"  => OpInfo::right(5),
        "="   => OpInfo::left(4),
        ":="  => OpInfo::left(3),
        "div" => OpInfo::left(7),
        "mod" => OpInfo::left(7),
        "*"   => OpInfo::left(7),
        "/"   => OpInfo::left(7),
        "+"   => OpInfo::left(6),
        "-"   => OpInfo::left(6),
        "<"   => OpInfo::left(4),
        ">"   => OpInfo::left(4),
        "<="  => OpInfo::left(4),
        ">="  => OpInfo::left(4),
      },
    }
  }

  pub(crate) fn start(&mut self) -> Marker {
    let pos = self.events.len();
    self.events.push(Event::Placeholder);

    Marker::new(pos)
  }

  pub(crate) fn parse(mut self) -> Vec<Event> {
    grammar::root(&mut self);
    self.events
  }

  /// Adds the current token to the current branch of the parse tree
  pub(crate) fn bump(&mut self) {
    self.expected_kinds.clear();
    self.source.next_token().unwrap(); // Should be fine as we only bump when not at EOF
    self.events.push(Event::AddToken);
  }

  pub(crate) fn expect(&mut self, kind: TokenKind) {
    if self.at(kind) {
      self.bump();
    } else {
      self.error();
    }
  }

  pub(crate) fn expect_set(&mut self, set: &[TokenKind]) {
    if self.at_set(set) {
      self.bump();
    } else {
      self.error();
    }
  }

  pub(crate) fn error(&mut self) {
    let (found, range) = if let Some(Token { kind, range, .. }) = self.source.peek_token() {
      (Some(*kind), *range)
    } else {
      (
        None,
        // If we're at the end of output just report it as the last token's range
        self
          .source
          .last_token_range()
          .expect("Only None when input is empty; grammar::root should gate this"),
      )
    };

    self.events.push(Event::Error(ParseError {
      expected: mem::take(&mut self.expected_kinds),
      found,
      range,
    }));

    if !self.at_set(&RECOVERY_SET) && !self.at_end() {
      let m = self.start();
      self.bump();
      m.complete(self, SyntaxKind::ERROR);
    }
  }

  /// Peeks at the `SyntaxKind` of the next token
  fn peek(&mut self) -> Option<TokenKind> {
    self.source.peek_kind()
  }

  pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
    self.expected_kinds.push(kind);
    self.peek() == Some(kind)
  }

  pub(crate) fn at_set(&mut self, set: &[TokenKind]) -> bool {
    self.peek().map_or(false, |k| set.contains(&k))
  }

  pub(crate) fn at_end(&mut self) -> bool {
    self.peek().is_none()
  }

  pub(crate) fn at_rem(&mut self, kind: TokenKind) -> bool {
    if self.at(kind) {
      p.bump();
    }
    self.at(kind)
  }

  pub(crate) fn at_op(&mut self) -> Result<&OpInfo, ()> {
    let Token { text, .. } = self.source.peek_token().ok_or(())?;
    todo!(r#"probably should add a "infix operator" class to `expected_kinds` on failure?"#);
    self.ops.get(text).ok_or(())
  }
}
