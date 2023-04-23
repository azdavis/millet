//! Dynamics types.

use fast_hash::FxHashMap;
use sml_hir::{la_arena, Lab, SCon};
use sml_statics_types::info::IdStatusMap;
use sml_statics_types::sym::Exn;
use std::collections::BTreeMap;
use str_util::Name;

#[derive(Debug, Clone)]
pub(crate) enum Val {
  SCon(SCon),
  Con(Con),
  Record(BTreeMap<Lab, Val>),
  Closure(Env, Vec<sml_hir::Arm>),
}

#[derive(Debug, Clone)]
pub(crate) enum ConKind {
  Dat(Name),
  Exn(Name, Exn),
}

#[derive(Debug, Clone)]
pub(crate) struct Con {
  pub(crate) kind: ConKind,
  pub(crate) arg: Option<Box<Val>>,
}

impl Con {
  pub(crate) fn empty(kind: ConKind) -> Self {
    Self { kind, arg: None }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Env {
  pub(crate) str: StrEnv,
  pub(crate) val: ValEnv,
}

impl Env {
  pub(crate) fn get<'e, 'n>(&'e self, names: &'n [Name]) -> Result<&'e Env, &'n Name> {
    let mut ret = self;
    for name in names {
      ret = match ret.str.get(name) {
        Some(x) => x,
        None => return Err(name),
      };
    }
    Ok(ret)
  }
}

pub(crate) type StrEnv = FxHashMap<Name, Env>;
pub(crate) type ValEnv = FxHashMap<Name, Val>;

#[derive(Debug)]
pub(crate) enum Step {
  Exp(la_arena::Idx<sml_hir::Exp>),
  Val(Val),
  Raise(Exception),
  Dec(sml_hir::DecIdx),
}

impl Step {
  pub(crate) fn exp(idx: sml_hir::ExpIdx) -> Self {
    Self::Exp(idx.expect("no exp"))
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Exception {
  pub(crate) name: Name,
  pub(crate) exn: Exn,
  pub(crate) arg: Option<Box<Val>>,
}

impl TryFrom<Con> for Exception {
  type Error = (Name, Option<Box<Val>>);

  fn try_from(con: Con) -> Result<Self, Self::Error> {
    let arg = con.arg;
    match con.kind {
      ConKind::Dat(name) => Err((name, arg)),
      ConKind::Exn(name, exn) => Ok(Self { name, exn, arg }),
    }
  }
}

impl From<Exception> for Con {
  fn from(exn: Exception) -> Self {
    Con { kind: ConKind::Exn(exn.name, exn.exn), arg: exn.arg }
  }
}

#[derive(Debug)]
pub(crate) struct Frame {
  pub(crate) env: Env,
  pub(crate) kind: FrameKind,
}

impl Frame {
  pub(crate) fn new(env: Env, kind: FrameKind) -> Self {
    Self { env, kind }
  }
}

#[derive(Debug)]
pub(crate) enum FrameKind {
  Record(BTreeMap<Lab, Val>, Lab, Vec<(Lab, sml_hir::ExpIdx)>),
  AppFunc(sml_hir::ExpIdx),
  AppArg(Vec<sml_hir::Arm>),
  Raise,
  Handle(Vec<sml_hir::Arm>),
  Let(Vec<sml_hir::DecIdx>, sml_hir::ExpIdx),
  ValBind(sml_hir::PatIdx),
  Local(Vec<sml_hir::DecIdx>, Vec<sml_hir::DecIdx>),
  In(Vec<sml_hir::DecIdx>),
}

/// A context under which we run dynamics.
#[derive(Debug, Clone, Copy)]
pub struct Cx<'a> {
  /// The arenas for HIR.
  pub ars: &'a sml_hir::Arenas,
  /// A mapping from path expressions to what kind of identifier status those paths are.
  pub exp: &'a IdStatusMap<sml_hir::Exp>,
  /// A mapping from path patterns to what kind of identifier status those paths are.
  pub pat: &'a IdStatusMap<sml_hir::Pat>,
  /// The built-in `Bind` exception.
  pub bind: Exn,
  /// The built-in `Match` exception.
  pub match_: Exn,
}

impl Cx<'_> {
  pub(crate) fn match_exn(&self) -> Exception {
    Exception { name: Name::new("Match"), exn: self.match_, arg: None }
  }

  pub(crate) fn bind_exn(&self) -> Exception {
    Exception { name: Name::new("Bind"), exn: self.bind, arg: None }
  }
}

#[derive(Debug, Default)]
pub(crate) struct St {
  pub(crate) env: Env,
  pub(crate) frames: Vec<Frame>,
}

impl St {
  pub(crate) fn push_with_cur_env(&mut self, kind: FrameKind) {
    let env = self.env.clone();
    self.frames.push(Frame::new(env, kind));
  }
}
