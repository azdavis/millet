//! High-level Intermediate Representation for SML.

#![deny(missing_debug_implementations)]
#![deny(rust_2018_idioms)]

use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use std::fmt;

pub use la_arena;

#[derive(Debug, Default)]
pub struct Arenas {
  pub exp: ExpArena,
  pub dec: DecArena,
  pub pat: PatArena,
  pub ty: TyArena,
}

pub type ExpIdx = Idx<Exp>;
pub type ExpArena = Arena<Exp>;

#[derive(Debug)]
pub enum Exp {
  None,
  SCon(SCon),
  Path(Path),
  Record(Vec<(Lab, ExpIdx)>),
  Let(DecIdx, ExpIdx),
  App(ExpIdx, ExpIdx),
  Handle(ExpIdx, Vec<(PatIdx, ExpIdx)>),
  Raise(ExpIdx),
  Fn(Vec<(PatIdx, ExpIdx)>),
  Typed(ExpIdx, TyIdx),
}

pub type DecIdx = Idx<Dec>;
pub type DecArena = Arena<Dec>;

#[derive(Debug)]
pub enum Dec {
  None,
  Val(Vec<TyVar>, Vec<ValBind>),
  Ty(Vec<TyBind>),
  Datatype(Vec<DatBind>),
}

#[derive(Debug)]
pub struct ValBind {
  pub rec: bool,
  pub pat: PatIdx,
  pub exp: ExpIdx,
}

#[derive(Debug)]
pub struct TyBind {
  pub ty_vars: Vec<TyVar>,
  pub name: Name,
  pub ty: TyIdx,
}

#[derive(Debug)]
pub struct DatBind {
  pub ty_vars: Vec<TyVar>,
  pub name: Name,
  pub cons: Vec<(Name, Option<TyIdx>)>,
}

pub type PatIdx = Idx<Pat>;
pub type PatArena = Arena<Pat>;

#[derive(Debug)]
pub enum Pat {
  None,
  Wild,
  SCon(SCon),
  Con(Path, Option<PatIdx>),
  Record {
    pats: Vec<(Lab, PatIdx)>,
    allows_other: bool,
  },
  Typed(PatIdx, TyIdx),
  As(Name, Option<TyIdx>, PatIdx),
}

pub type TyIdx = Idx<Ty>;
pub type TyArena = Arena<Ty>;

#[derive(Debug)]
pub enum Ty {
  None,
}

#[derive(Debug)]
pub enum Lab {
  Name(Name),
  Num(usize),
}

#[derive(Debug)]
pub enum SCon {
  Int(i32),
  Real(f64),
  Word(i32),
  Char(u8),
  String(SmolStr),
}

#[derive(Debug)]
pub struct Path(Vec<Name>);

impl Path {
  pub fn new(names: Vec<Name>) -> Self {
    assert!(!names.is_empty());
    Self(names)
  }

  pub fn last(&self) -> &Name {
    self.0.last().unwrap()
  }
}

#[derive(Debug, Clone)]
pub struct Name(SmolStr);

impl Name {
  pub fn new<S>(s: S) -> Self
  where
    S: Into<SmolStr>,
  {
    let s = s.into();
    assert!(!s.is_empty());
    Self(s)
  }

  pub fn as_str(&self) -> &str {
    self.0.as_str()
  }
}

impl From<Name> for Path {
  fn from(name: Name) -> Path {
    Path::new(vec![name])
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug)]
pub struct TyVar(SmolStr);

impl TyVar {
  pub fn new(s: &str) -> Self {
    assert!(s.len() >= 2);
    assert!(s.as_bytes()[0] == b'\'');
    Self(s.into())
  }

  pub fn is_equality(&self) -> bool {
    self.0.as_bytes()[1] == b'\''
  }

  pub fn as_str(&self) -> &str {
    self.0.as_str()
  }
}

impl fmt::Display for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}
