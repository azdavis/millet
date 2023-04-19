//! Mid-level Intermediate Representation. Aka, types for the dynamic semantics.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

use std::collections::BTreeMap;
use uniq::Uniq;

pub use sml_lab::Lab;
pub use sml_scon::SCon;

#[derive(Debug, Clone)]
pub enum Exp {
  SCon(SCon),
  Var(Uniq),
  Con(Uniq),
  Record(Vec<(Lab, Exp)>),
  Let(Vec<Dec>, Box<Exp>),
  App(Box<Exp>, Box<Exp>),
  Handle(Box<Exp>, Vec<Arm>),
  Raise(Box<Exp>),
  Fn(Vec<Arm>),
}

#[derive(Debug, Clone)]
pub struct Arm {
  pub pat: Pat,
  pub exp: Exp,
}

#[derive(Debug, Clone)]
pub enum Dec {
  Val(Vec<ValBind>),
  Datatype(Vec<DatBind>),
  DatatypeCopy(Uniq, Uniq),
  Exception(Vec<ExBind>),
  Local(Vec<Dec>, Vec<Dec>),
}

#[derive(Debug, Clone)]
pub struct ValBind {
  pub rec: bool,
  pub pat: Pat,
  pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct DatBind {
  pub ty_vars: usize,
  pub name: Uniq,
  pub cons: Vec<ConBind>,
}

#[derive(Debug, Clone)]
pub struct ConBind {
  pub name: Uniq,
  pub ty: bool,
}

#[derive(Debug, Clone)]
pub enum ExBind {
  /// The bool is whether this has an `of ty`.
  New(Uniq, bool),
  Copy(Uniq, Uniq),
}

#[derive(Debug, Clone)]
pub enum Pat {
  Wild,
  Var(Uniq),
  SCon(SCon),
  Con(Uniq, Option<Box<Pat>>),
  Record { rows: BTreeMap<Lab, Pat>, allows_other: bool },
  As(Uniq, Box<Pat>),
  Or(Box<Pat>, Vec<Pat>),
}
