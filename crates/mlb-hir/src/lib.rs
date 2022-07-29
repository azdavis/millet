//! HIR for ML Basis.

#![deny(missing_debug_implementations, rust_2018_idioms)]

use located::Located;

#[derive(Debug)]
pub enum BasDec {
  Basis(Located<hir::Name>, Box<BasExp>),
  Open(Located<hir::Name>),
  Local(Box<BasDec>, Box<BasDec>),
  Export(Namespace, Located<hir::Name>, Located<hir::Name>),
  Seq(Vec<BasDec>),
  Path(paths::PathId, PathKind),
}

impl BasDec {
  pub fn seq(mut decs: Vec<Self>) -> Self {
    if decs.len() == 1 {
      decs.pop().unwrap()
    } else {
      Self::Seq(decs)
    }
  }
}

#[derive(Debug)]
pub enum BasExp {
  Bas(BasDec),
  Name(Located<hir::Name>),
  Let(BasDec, Box<BasExp>),
}

#[derive(Debug, Clone, Copy)]
pub enum Namespace {
  Structure,
  Signature,
  Functor,
}

#[derive(Debug, Clone, Copy)]
pub enum PathKind {
  Sml,
  Mlb,
}
