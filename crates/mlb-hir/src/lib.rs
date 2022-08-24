//! HIR for ML Basis.

#![deny(missing_debug_implementations, rust_2018_idioms)]

use text_size_util::WithRange;

#[derive(Debug, Clone)]
pub enum BasDec {
  Basis(WithRange<sml_hir::Name>, Box<BasExp>),
  Open(WithRange<sml_hir::Name>),
  Local(Box<BasDec>, Box<BasDec>),
  Export(
    Namespace,
    WithRange<sml_hir::Name>,
    WithRange<sml_hir::Name>,
  ),
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

#[derive(Debug, Clone)]
pub enum BasExp {
  Bas(BasDec),
  Name(WithRange<sml_hir::Name>),
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
