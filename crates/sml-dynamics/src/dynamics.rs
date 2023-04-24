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
  pub fn new(cx: Cx<'a>, mut decs: Vec<sml_hir::DecIdx>) -> Option<Self> {
    let mut st = St::default();
    decs.reverse();
    let dec = decs.pop()?;
    st.push_with_cur_env(FrameKind::Local(decs, Vec::new()));
    Some(Self { cx, st, step: Some(Step::Dec(dec)) })
  }

  /// Takes a step. Panics if this was already finished.
  #[must_use]
  pub fn step(mut self) -> Progress<'a> {
    let mut s = self.step.take().expect("no step");
    s = step(&mut self.st, self.cx, s);
    if self.st.frames.is_empty() {
      match s {
        Step::Raise(_) => Progress::Raise,
        Step::Val(_) | Step::Exp(_) | Step::Dec(_) => unreachable!("not done, but no frames"),
        Step::DecDone => Progress::Done,
      }
    } else {
      self.step = Some(s);
      Progress::Still(self)
    }
  }
}

/// A way for the dynamics to progress.
#[derive(Debug)]
pub enum Progress<'a> {
  /// Still evaluating.
  Still(Dynamics<'a>),
  /// Done evaluating.
  Done,
  /// Raised an exception.
  Raise,
}
