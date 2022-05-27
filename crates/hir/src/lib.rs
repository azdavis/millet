//! High-level Intermediate Representation.

#![deny(missing_debug_implementations)]
#![deny(rust_2018_idioms)]

use la_arena::{Arena, Idx};
use std::fmt;

pub use la_arena;
pub use smol_str::SmolStr;

#[derive(Debug, Default)]
pub struct Arenas {
  pub str_dec: StrDecArena,
  pub str_exp: StrExpArena,
  pub sig_exp: SigExpArena,
  pub spec: SpecArena,
  pub exp: ExpArena,
  pub dec: DecArena,
  pub pat: PatArena,
  pub ty: TyArena,
}

// modules //

#[derive(Debug)]
pub enum TopDec {
  Str(StrDecIdx),
  Sig(Vec<SigBind>),
  Functor(Vec<FunctorBind>),
}

#[derive(Debug)]
pub struct SigBind {
  pub name: Name,
  pub sig_exp: SigExpIdx,
}

#[derive(Debug)]
pub struct FunctorBind {
  pub functor_name: Name,
  pub param_name: Name,
  pub param_sig: SigExpIdx,
  pub body: StrExpIdx,
}

pub type StrDecIdx = Option<Idx<StrDec>>;
pub type StrDecArena = Arena<StrDec>;

#[derive(Debug)]
pub enum StrDec {
  Dec(DecIdx),
  Structure(Vec<StrBind>),
  Local(StrDecIdx, StrDecIdx),
  Seq(Vec<StrDecIdx>),
}

#[derive(Debug)]
pub struct StrBind {
  pub name: Name,
  pub str_exp: StrExpIdx,
}

pub type StrExpIdx = Option<Idx<StrExp>>;
pub type StrExpArena = Arena<StrExp>;

#[derive(Debug)]
pub enum StrExp {
  Struct(StrDecIdx),
  Path(Path),
  Ascription(StrExpIdx, Ascription, SigExpIdx),
  App(Name, StrExpIdx),
  Let(StrDecIdx, StrExpIdx),
}

#[derive(Debug)]
pub enum Ascription {
  Transparent,
  Opaque,
}

pub type SigExpIdx = Option<Idx<SigExp>>;
pub type SigExpArena = Arena<SigExp>;

#[derive(Debug)]
pub enum SigExp {
  Spec(SpecIdx),
  Name(Name),
  Where(SigExpIdx, Vec<TyVar>, Path, TyIdx),
}

pub type SpecIdx = Option<Idx<Spec>>;
pub type SpecArena = Arena<Spec>;

#[derive(Debug)]
pub enum Spec {
  Val(Vec<ValDesc>),
  Ty(Vec<TyDesc>),
  EqTy(Vec<TyDesc>),
  Datatype(Vec<DatDesc>),
  DatatypeCopy(Name, Path),
  Exception(Vec<ExDesc>),
  Str(Vec<StrDesc>),
  Include(SigExpIdx),
  Sharing(SpecIdx, Vec<Path>),
  Seq(Vec<SpecIdx>),
}

#[derive(Debug)]
pub struct ValDesc {
  pub name: Name,
  pub ty: TyIdx,
}

#[derive(Debug)]
pub struct TyDesc {
  pub ty_vars: Vec<TyVar>,
  pub name: Name,
}

pub type DatDesc = DatBind;
pub type ConDesc = ConBind;

#[derive(Debug)]
pub struct ExDesc {
  pub name: Name,
  pub ty: Option<TyIdx>,
}

#[derive(Debug)]
pub struct StrDesc {
  pub name: Name,
  pub sig_exp: SigExpIdx,
}

// core //

pub type ExpIdx = Option<Idx<Exp>>;
pub type ExpArena = Arena<Exp>;

#[derive(Debug)]
pub enum Exp {
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

pub type DecIdx = Option<Idx<Dec>>;
pub type DecArena = Arena<Dec>;

#[derive(Debug)]
pub enum Dec {
  Val(Vec<TyVar>, Vec<ValBind>),
  Ty(Vec<TyBind>),
  Datatype(Vec<DatBind>),
  DatatypeCopy(Name, Path),
  Abstype(Vec<DatBind>, DecIdx),
  Exception(Vec<ExBind>),
  Local(DecIdx, DecIdx),
  Open(Vec<Path>),
  Seq(Vec<DecIdx>),
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
  pub cons: Vec<ConBind>,
}

#[derive(Debug)]
pub struct ConBind {
  pub name: Name,
  pub ty: Option<TyIdx>,
}

#[derive(Debug)]
pub enum ExBind {
  New(Name, Option<TyIdx>),
  Copy(Name, Path),
}

pub type PatIdx = Option<Idx<Pat>>;
pub type PatArena = Arena<Pat>;

#[derive(Debug)]
pub enum Pat {
  Wild,
  SCon(SCon),
  Con(Path, Option<PatIdx>),
  Record {
    rows: Vec<(Lab, PatIdx)>,
    allows_other: bool,
  },
  Typed(PatIdx, TyIdx),
  /// the Definition defines as-pats as having a built-in optional type annotation. however, it
  /// appears that lowering `vid : ty as pat -> vid as (pat : ty)` should be equivalent modulo
  /// possible slight error message differences. this lets us avoid the optional type annotation in
  /// the HIR def for as-pats and instead handle it in lowering.
  As(Name, PatIdx),
}

pub type TyIdx = Option<Idx<Ty>>;
pub type TyArena = Arena<Ty>;

#[derive(Debug)]
pub enum Ty {
  Var(TyVar),
  Record(Vec<(Lab, TyIdx)>),
  Con(Vec<TyIdx>, Path),
  Fn(TyIdx, TyIdx),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lab {
  Name(Name),
  Num(usize),
}

impl fmt::Display for Lab {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Name(name) => name.fmt(f),
      Self::Num(n) => n.fmt(f),
    }
  }
}

impl Lab {
  pub fn tuple(idx: usize) -> Self {
    Self::Num(idx + 1)
  }
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
pub struct Path {
  structures: Vec<Name>,
  last: Name,
}

impl Path {
  pub fn try_new(mut names: Vec<Name>) -> Option<Self> {
    let last = names.pop()?;
    Some(Self {
      structures: names,
      last,
    })
  }

  pub fn one(name: Name) -> Self {
    Self {
      structures: Vec::new(),
      last: name,
    }
  }

  pub fn last(&self) -> &Name {
    &self.last
  }

  pub fn structures(&self) -> &[Name] {
    &self.structures
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar(SmolStr);

impl TyVar {
  pub fn new(s: &str) -> Self {
    assert!(s.len() >= 2);
    assert_eq!(s.as_bytes()[0], b'\'');
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
