//! The overall dynamics type.

use crate::step::step;
use crate::types::{Cx, FrameKind, St, Step};

/// A runner of the dynamics.
#[derive(Debug)]
pub struct Dynamics<'a> {
  pub(crate) cx: Cx<'a>,
  pub(crate) st: St,
  pub(crate) step: Option<Step>,
}

impl<'a> Dynamics<'a> {
  /// Returns a new dynamics for these decs, or `None` if `decs` is empty.
  #[must_use]
  pub fn new(cx: Cx<'a>, decs: Vec<sml_hir::DecIdx>) -> Option<Self> {
    let mut st = St::default();
    let mut decs = decs.into_iter();
    let dec = decs.next()?;
    st.push_with_cur_env(FrameKind::Local(decs, Vec::new().into_iter()));
    Some(Self { cx, st, step: Some(Step::Dec(dec)) })
  }

  /// Takes a step, or no-ops if it's done.
  pub fn step(&mut self) {
    let s = self.step.take().expect("no step");
    self.step = Some(step(&mut self.st, self.cx, s));
  }
}
