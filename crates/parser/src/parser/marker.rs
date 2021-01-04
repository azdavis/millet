use super::Parser;
use crate::event::Event;
use drop_bomb::DropBomb;
use syntax::SyntaxKind;

pub(crate) struct Marker {
  pos: usize,
  // Used to enforce linear typing for Marker at runtime. Defused when a CompletedMarker is
  // created from this one.
  bomb: DropBomb,
}

impl Marker {
  pub(crate) fn new(pos: usize) -> Self {
    Self {
      pos,
      bomb: DropBomb::new("Markers must be completed"),
    }
  }

  pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
    self.bomb.defuse();

    let event_at_pos = &mut p.events[self.pos];
    assert_eq!(*event_at_pos, Event::Placeholder);
    *event_at_pos = Event::StartNode {
      kind,
      forward_parent: None,
    };

    p.events.push(Event::FinishNode);

    CompletedMarker {
      pos: self.pos,
      kind,
    }
  }

  pub(crate) fn abandon(mut self, p: &mut Parser) {
    self.bomb.defuse();
    let idx = self.pos;
    if idx == p.events.len() - 1 {
      match p.events.pop() {
        Some(Event::Placeholder) => (),
        _ => unreachable!(),
      }
    }
  }
}

#[derive(Debug)]
pub(crate) struct CompletedMarker {
  pos: usize,
  kind: SyntaxKind,
}

impl CompletedMarker {
  pub(crate) fn precede(self, p: &mut Parser) -> Marker {
    let new_m = p.start();

    if let Event::StartNode {
      ref mut forward_parent,
      ..
    } = p.events[self.pos]
    {
      *forward_parent = Some(new_m.pos - self.pos);
    } else {
      unreachable!();
    }

    new_m
  }

  pub(crate) fn kind(&self) -> SyntaxKind {
    self.kind
  }
}
