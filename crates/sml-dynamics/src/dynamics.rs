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
  pub fn step(&mut self) -> Progress {
    let mut s = self.step.take().expect("already done");
    s = step(&mut self.st, self.cx, s);
    if self.st.frames.is_empty() {
      match s {
        Step::Val(_) => Progress::Val,
        Step::Raise(_) => Progress::Raise,
        Step::Exp(_) | Step::Dec(_) => unreachable!("can't be done with Exp or Dec"),
      }
    } else {
      self.step = Some(s);
      Progress::Still
    }
  }
}

/// A way for the dynamics to progress
#[derive(Debug, Clone, Copy)]
pub enum Progress {
  /// Still evaluating.
  Still,
  /// Returned a value.
  Val,
  /// Raised an exception.
  Raise,
}
