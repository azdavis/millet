//! The overall dynamics type.

use crate::step::step;
use crate::types::{Cx, St, Step};

/// A runner of the dynamics.
#[derive(Debug)]
pub struct Dynamics<'a> {
  cx: Cx<'a>,
  st: St,
  step: Option<Step>,
}

impl<'a> Dynamics<'a> {
  /// Returns a new dynamics for this dec.
  #[must_use]
  pub fn new(cx: Cx<'a>, dec: sml_hir::DecIdx) -> Self {
    Self { cx, st: St::default(), step: Some(Step::Dec(dec)) }
  }

  /// Takes a step, or no-ops if it's done.
  pub fn step(&mut self) {
    let s = self.step.take().expect("no step");
    self.step = Some(step(&mut self.st, self.cx, s));
  }
}
