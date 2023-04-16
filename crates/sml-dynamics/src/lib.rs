//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]
// TODO remove
#![allow(dead_code)]

use sml_path::Path;
use std::collections::BTreeMap;
use str_util::Name;

#[derive(Debug)]
enum Exp {
  SCon(sml_hir::SCon),
  Path(Path),
  Record(BTreeMap<sml_hir::Lab, Exp>),
  Let(Vec<Dec>, Box<Exp>),
  App(Box<Exp>, Box<Exp>),
  Handle(Box<Exp>, Vec<(Pat, Exp)>),
  Raise(Box<Exp>),
  Fn(Vec<(Pat, Exp)>),
}

#[derive(Debug)]
struct Arm {
  pat: Pat,
  exp: Exp,
}

#[derive(Debug)]
enum Dec {
  Val(Vec<ValBind>),
  Datatype(Vec<DatBind>),
  DatatypeCopy(Name, Path),
  Exception(Vec<ExBind>),
  Local(Vec<Dec>, Vec<Dec>),
  Open(Vec<Path>),
}

#[derive(Debug)]
struct ValBind {
  rec: bool,
  pat: Pat,
  exp: Exp,
}

#[derive(Debug)]
struct DatBind {
  ty_vars: usize,
  name: Name,
  cons: Vec<ConBind>,
}

#[derive(Debug)]
struct ConBind {
  name: Name,
  ty: bool,
}

#[derive(Debug)]
enum ExBind {
  /// The bool is whether this has an `of ty`.
  New(Name, bool),
  Copy(Name, Path),
}

#[derive(Debug)]
enum Pat {
  Wild,
  SCon(sml_hir::SCon),
  Con(Path, Option<Box<Pat>>),
  Record { rows: Vec<(sml_hir::Lab, Pat)>, allows_other: bool },
  As(Name, Box<Pat>),
  Or(Box<Pat>, Vec<Pat>),
}
