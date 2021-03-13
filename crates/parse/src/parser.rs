//! A fork of event-parse from language-server-util, with SML-specific functionality.
//!
//! This includes:
//!
//! - Unbounded backtracking
//! - Dynamic operator precedence
//! - Custom errors (not just expected ..., found ...)
//!
//! Since it's not a library, we can also drop things like
//!
//! - the `Sink` trait
//! - the type parameter for the `SyntaxKind`
//! - the various bounds on the methods
//!
//! Feels a little bad to fork event-parse, but as I saw it, since the SML grammar has enough
//! oddities that it doesn't _quite_ fit, we'd either need to shove them into event-parse or fork.
//! And the library isn't even that big, so forking is... okay.

use drop_bomb::DropBomb;
use rustc_hash::FxHashMap;
use std::fmt;
use syntax::rowan::{GreenNodeBuilder, TextRange, TextSize};
use syntax::token::{Token, Triviable};
use syntax::{SyntaxKind as SK, SyntaxNode};

/// A event-based parser for SML.
#[derive(Debug)]
pub(crate) struct Parser<'input> {
  tokens: &'input [Token<'input, SK>],
  idx: usize,
  expected: Vec<SK>,
  events: Vec<Option<Event>>,
  ops: FxHashMap<&'input str, OpInfo>,
}

impl<'input> Parser<'input> {
  pub(crate) fn new(tokens: &'input [Token<'input, SK>]) -> Self {
    let mut ops = FxHashMap::default();
    ops.insert("::", OpInfo::right(5));
    ops.insert("=", OpInfo::left(4));
    ops.insert(":=", OpInfo::left(3));
    ops.insert("div", OpInfo::left(7));
    ops.insert("mod", OpInfo::left(7));
    ops.insert("*", OpInfo::left(7));
    ops.insert("/", OpInfo::left(7));
    ops.insert("+", OpInfo::left(6));
    ops.insert("-", OpInfo::left(6));
    ops.insert("<", OpInfo::left(4));
    ops.insert(">", OpInfo::left(4));
    ops.insert("<=", OpInfo::left(4));
    ops.insert(">=", OpInfo::left(4));
    Self {
      tokens,
      idx: 0,
      expected: Vec::new(),
      events: Vec::new(),
      ops,
    }
  }

  pub(crate) fn enter(&mut self) -> Entered {
    let idx = self.events.len();
    self.events.push(None);
    Entered {
      bomb: DropBomb::new("Entered markers must be exited"),
      idx,
    }
  }

  pub(crate) fn abandon(&mut self, mut entered: Entered) {
    entered.bomb.defuse();
    assert!(self.events[entered.idx].is_none());
  }

  pub(crate) fn exit(&mut self, mut entered: Entered, kind: SK) -> Exited {
    entered.bomb.defuse();
    let ev = &mut self.events[entered.idx];
    assert!(ev.is_none());
    *ev = Some(Event::Enter(kind, None));
    self.events.push(Some(Event::Exit));
    Exited { idx: entered.idx }
  }

  pub(crate) fn precede(&mut self, exited: Exited) -> Entered {
    let ret = self.enter();
    match self.events[exited.idx] {
      Some(Event::Enter(_, ref mut parent)) => {
        assert!(parent.is_none());
        *parent = Some(ret.idx);
      }
      _ => unreachable!("{:?} did not precede an Enter", exited),
    }
    ret
  }

  pub(crate) fn save(&mut self) -> Save {
    Save {
      idx: self.idx,
      events_len: self.events.len(),
      expected: std::mem::take(&mut self.expected),
    }
  }

  pub(crate) fn error_since(&self, save: &Save) -> bool {
    self
      .events
      .iter()
      .skip(save.events_len)
      .any(|ev| matches!(*ev, Some(Event::Expected(..))))
  }

  pub(crate) fn restore(&mut self, save: Save) {
    self.idx = save.idx;
    self.events.truncate(save.events_len);
    self.expected = save.expected;
  }

  pub(crate) fn insert_op(&mut self, k: &'input str, v: OpInfo) {
    self.ops.insert(k, v);
  }

  pub(crate) fn get_op(&self, k: &str) -> Option<OpInfo> {
    self.ops.get(k).copied()
  }

  pub(crate) fn contains_op(&self, k: &str) -> bool {
    self.ops.contains_key(k)
  }

  pub(crate) fn remove_op(&mut self, k: &str) {
    self.ops.remove(k);
  }

  pub(crate) fn peek(&mut self) -> Option<Token<'input, SK>> {
    while let Some(&tok) = self.tokens.get(self.idx) {
      if tok.kind.is_trivia() {
        self.idx += 1;
      } else {
        return Some(tok);
      }
    }
    None
  }

  pub(crate) fn peek_n(&mut self, n: usize) -> Option<Token<'input, SK>> {
    let mut ret = self.peek();
    let idx = self.idx;
    for _ in 0..n {
      self.idx += 1;
      ret = self.peek();
    }
    self.idx = idx;
    ret
  }

  pub(crate) fn bump(&mut self) -> Token<'input, SK> {
    let ret = self.peek().expect("bump with no tokens");
    self.events.push(Some(Event::Token));
    self.idx += 1;
    self.expected.clear();
    ret
  }

  pub(crate) fn error(&mut self) {
    self._error(None)
  }

  pub(crate) fn error_with(&mut self, message: String) {
    self._error(Some(message))
  }

  fn _error(&mut self, message: Option<String>) {
    let expected = std::mem::take(&mut self.expected);
    if self.peek().is_some() {
      self.bump();
    }
    self.events.push(Some(Event::Expected(expected, message)));
  }

  fn eat_trivia(&mut self, sink: &mut BuilderSink) {
    while let Some(&tok) = self.tokens.get(self.idx) {
      if !tok.kind.is_trivia() {
        break;
      }
      sink.token(tok);
      self.idx += 1;
    }
  }

  pub(crate) fn finish(mut self) -> (SyntaxNode, Vec<Error>) {
    self.idx = 0;
    let mut kinds = Vec::new();
    let mut levels: usize = 0;
    let mut sink = BuilderSink::default();
    for idx in 0..self.events.len() {
      let ev = match self.events[idx].take() {
        Some(ev) => ev,
        None => continue,
      };
      match ev {
        Event::Enter(kind, mut parent) => {
          assert!(kinds.is_empty());
          kinds.push(kind);
          while let Some(p) = parent {
            match self.events[p].take() {
              Some(Event::Enter(kind, new_parent)) => {
                kinds.push(kind);
                parent = new_parent;
              }
              _ => unreachable!("{:?} was not an Enter", parent),
            }
          }
          for kind in kinds.drain(..).rev() {
            // keep as much trivia as possible outside of what we're entering.
            if levels != 0 {
              self.eat_trivia(&mut sink);
            }
            sink.enter(kind);
            levels += 1;
          }
        }
        Event::Exit => {
          sink.exit();
          levels -= 1;
          // keep as much trivia as possible outside of top-level items.
          if levels == 1 {
            self.eat_trivia(&mut sink);
          }
        }
        Event::Token => {
          self.eat_trivia(&mut sink);
          sink.token(self.tokens[self.idx]);
          self.idx += 1;
        }
        Event::Expected(expected, message) => sink.error(expected, message),
      }
    }
    assert_eq!(levels, 0);
    (SyntaxNode::new_root(sink.builder.finish()), sink.errors)
  }

  pub(crate) fn at(&mut self, kind: SK) -> bool {
    self.expected.push(kind);
    self.peek().map_or(false, |tok| tok.kind == kind)
  }

  pub(crate) fn eat(&mut self, kind: SK) -> Option<Token<'input, SK>> {
    if self.at(kind) {
      Some(self.bump())
    } else {
      self.error();
      None
    }
  }
}

/// A marker for a syntax construct that is mid-parse. If this is not consumed
/// by a [`Parser`], it will panic when dropped.
#[derive(Debug)]
pub(crate) struct Entered {
  bomb: DropBomb,
  idx: usize,
}

/// A marker for a syntax construct that has been fully parsed.
#[derive(Debug)]
pub(crate) struct Exited {
  idx: usize,
}

/// The saved state of the parser.
#[derive(Debug)]
pub(crate) struct Save {
  idx: usize,
  events_len: usize,
  expected: Vec<SK>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct OpInfo {
  pub num: usize,
  pub assoc: Assoc,
}

impl OpInfo {
  /// Returns a new OpInfo with left associativity.
  pub(crate) fn left(num: usize) -> Self {
    Self {
      num,
      assoc: Assoc::Left,
    }
  }

  /// Returns a new OpInfo with right associativity.
  pub(crate) fn right(num: usize) -> Self {
    Self {
      num,
      assoc: Assoc::Right,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Assoc {
  Left,
  Right,
}

#[derive(Debug)]
enum Event {
  Enter(SK, Option<usize>),
  Token,
  Exit,
  Expected(Vec<SK>, Option<String>),
}

#[derive(Default)]
struct BuilderSink {
  builder: GreenNodeBuilder<'static>,
  range: Option<TextRange>,
  errors: Vec<Error>,
}

impl BuilderSink {
  fn enter(&mut self, kind: SK) {
    self.builder.start_node(kind.into());
  }

  fn token(&mut self, token: Token<'_, SK>) {
    self.builder.token(token.kind.into(), token.text);
    let start = self.range.as_ref().map_or(0.into(), |range| range.end());
    let end = start + TextSize::of(token.text);
    self.range = Some(TextRange::new(start, end));
  }

  fn exit(&mut self) {
    self.builder.finish_node();
  }

  fn error(&mut self, kinds: Vec<SK>, message: Option<String>) {
    self.errors.push(Error {
      range: self.range.clone().expect("error with no tokens"),
      expected: Expected { kinds },
      message,
    });
  }
}

/// A parse error.
#[derive(Debug)]
pub struct Error {
  /// The range of the unexpected token.
  pub range: TextRange,
  /// The tokens that would have been allowed.
  pub expected: Expected,
  /// The message (if any).
  pub message: Option<String>,
}

/// A list of expected tokens.
#[derive(Debug)]
pub struct Expected {
  /// The token kinds.
  pub kinds: Vec<SK>,
}

impl fmt::Display for Expected {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.kinds.iter();
    write!(f, "expected any of ")?;
    if let Some(kind) = iter.next() {
      write!(f, "{}", kind.token_desc().unwrap_or("<non-token>"))?;
    }
    for kind in iter {
      write!(f, ", {}", kind.token_desc().unwrap_or("<non-token>"))?;
    }
    Ok(())
  }
}
