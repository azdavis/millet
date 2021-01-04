use crate::{event::Event, parser::ParseError, Parse};
use lexer::Token;
use rowan::{GreenNodeBuilder, Language};
use std::mem;
use syntax::SML;

pub(crate) struct Sink<'l, 'input> {
  builder: GreenNodeBuilder<'static>,
  tokens: &'l [Token<'input>],
  cursor: usize,
  events: Vec<Event>,
  errors: Vec<ParseError>,
}

impl<'l, 'input> Sink<'l, 'input> {
  pub(crate) fn new(tokens: &'l [Token<'input>], events: Vec<Event>) -> Self {
    Self {
      builder: GreenNodeBuilder::new(),
      tokens,
      cursor: 0,
      events,
      errors: Vec::new(),
    }
  }

  pub(crate) fn finish(mut self) -> Parse {
    for idx in 0..self.events.len() {
      match mem::replace(&mut self.events[idx], Event::Placeholder) {
        Event::StartNode {
          kind,
          forward_parent,
        } => {
          let mut kinds = vec![kind];

          let mut idx = idx;
          let mut forward_parent = forward_parent;

          // Walk through the forward parent of the forward parent, and the forward parent
          // of that, and of that, etc. until we reach a StartNode event without a forward
          // parent.
          while let Some(fp) = forward_parent {
            idx += fp;

            forward_parent = if let Event::StartNode {
              kind,
              forward_parent,
            } = mem::replace(&mut self.events[idx], Event::Placeholder)
            {
              kinds.push(kind);
              forward_parent
            } else {
              unreachable!()
            };
          }

          for kind in kinds.into_iter().rev() {
            self.builder.start_node(SML::kind_to_raw(kind));
          }
        }
        Event::AddToken => self.token(),
        Event::FinishNode => self.builder.finish_node(),
        Event::Error(e) => self.errors.push(e),
        Event::Placeholder => {}
      }

      self.consume_trivia();
    }

    Parse {
      green_node: self.builder.finish(),
      errors: self.errors,
    }
  }

  fn consume_trivia(&mut self) {
    while let Some(token) = self.tokens.get(self.cursor) {
      if !token.kind.is_trivia() {
        break;
      }

      self.token();
    }
  }

  fn token(&mut self) {
    let Token { kind, text, .. } = self.tokens[self.cursor];

    self
      .builder
      .token(SML::kind_to_raw(kind.into()), text.into());

    self.cursor += 1;
  }
}
