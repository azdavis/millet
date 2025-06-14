//! Dynamics types.

use fast_hash::{FxHashMap, FxHashSet, map};
use sml_hir::{Lab, SCon, la_arena};
use sml_statics_types::info::IdStatusMap;
use sml_statics_types::sym::Exn;
use std::collections::BTreeMap;
use str_util::Name;

#[derive(Debug, Clone)]
pub(crate) enum Val {
  SCon(SCon),
  Con(Con),
  Vector(Vec<Val>),
  Record(BTreeMap<Lab, Val>),
  Closure(Closure),
  Builtin(Builtin),
}

impl Val {
  pub(crate) fn unwrap_pair(self) -> [Val; 2] {
    match self {
      Val::Record(mut rows) => {
        assert_eq!(rows.len(), 2);
        let fst = rows.remove(&Lab::tuple(0)).expect("should have fst");
        let snd = rows.remove(&Lab::tuple(1)).expect("should have snd");
        [fst, snd]
      }
      _ => unreachable!("not Record: {self:?}"),
    }
  }

  pub(crate) fn unwrap_scon(self) -> SCon {
    match self {
      Val::SCon(scon) => scon,
      _ => unreachable!("not SCon: {self:?}"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Builtin {
  Add,
}

impl Builtin {
  pub(crate) fn as_str(self) -> &'static str {
    match self {
      Builtin::Add => "+",
    }
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Closure {
  pub(crate) env: Env,
  pub(crate) this: FxHashSet<Name>,
  pub(crate) matcher: Vec<sml_hir::Arm>,
}

#[derive(Debug, Clone)]
pub(crate) enum ConKind {
  Dat,
  Exn(Exn),
}

#[derive(Debug, Clone)]
pub(crate) struct Con {
  pub(crate) name: Name,
  pub(crate) kind: ConKind,
  pub(crate) arg: Option<Box<Val>>,
}

impl Con {
  pub(crate) fn empty(name: Name, kind: ConKind) -> Self {
    Self { name, kind, arg: None }
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Env {
  pub(crate) str: StrEnv,
  pub(crate) val: ValEnv,
}

impl Env {
  pub(crate) fn empty() -> Env {
    Env { str: StrEnv::default(), val: ValEnv::default() }
  }

  pub(crate) fn std_basis() -> Env {
    Env { str: StrEnv::default(), val: map([(Name::new("+"), Val::Builtin(Builtin::Add))]) }
  }

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
  DecDone,
  StrDec(sml_hir::StrDecIdx),
}

impl Step {
  pub(crate) fn exp(idx: sml_hir::ExpIdx) -> Self {
    Self::Exp(idx.expect("should have exp"))
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
      ConKind::Dat => Err((con.name, arg)),
      ConKind::Exn(exn) => Ok(Self { name: con.name, exn, arg }),
    }
  }
}

impl From<Exception> for Con {
  fn from(exn: Exception) -> Self {
    Con { name: exn.name, kind: ConKind::Exn(exn.exn), arg: exn.arg }
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
  /// The bool is whether this is actually a tuple.
  Record(bool, BTreeMap<Lab, Val>, Lab, Vec<(Lab, sml_hir::ExpIdx)>),
  Vector(Vec<Val>, Vec<sml_hir::ExpIdx>),
  AppFunc(sml_hir::ExpIdx),
  AppClosureArg(Vec<sml_hir::Arm>),
  AppBuiltinArg(Builtin),
  AppConArg(Name, ConKind),
  Raise,
  Handle(Vec<sml_hir::Arm>),
  Let(Vec<sml_hir::DecIdx>, sml_hir::ExpIdx),
  /// The bool is whether this is recursive.
  ValBind(bool, sml_hir::PatIdx, Vec<sml_hir::ValBind>),
  Local(Vec<sml_hir::DecIdx>, Vec<sml_hir::DecIdx>),
  In(Vec<sml_hir::DecIdx>),
  DecSeq(Vec<sml_hir::DecIdx>),
  StrDecSeq(Vec<sml_hir::StrDecIdx>),
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

#[derive(Debug)]
pub(crate) struct St {
  pub(crate) env: Env,
  pub(crate) frames: Vec<Frame>,
}

impl St {
  pub(crate) fn new_with_std_basis() -> St {
    St { env: Env::std_basis(), frames: Vec::new() }
  }

  pub(crate) fn push_with_cur_env(&mut self, kind: FrameKind) {
    let env = self.env.clone();
    self.frames.push(Frame::new(env, kind));
  }
}
