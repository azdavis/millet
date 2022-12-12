//! HIR for MLB.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::FxHashSet;
use sml_namespace::Namespace;
use text_size_util::WithRange;

/// A basis declaration.
#[derive(Debug)]
pub enum BasDec {
  /// `basis <name> = <exp>`
  Basis(WithRange<str_util::Name>, Box<BasExp>),
  /// `open <name>`
  Open(WithRange<str_util::Name>),
  /// `local <dec> in <dec> end`
  Local(Box<BasDec>, Box<BasDec>),
  /// `structure <name>`, etc.
  Export(Namespace, WithRange<str_util::Name>, WithRange<str_util::Name>),
  /// A sequence of declarations.
  Seq(Vec<BasDec>),
  /// A file path.
  Path(paths::PathId, PathKind),
  /// Used by CM only.
  SourcePathSet(FxHashSet<paths::PathId>),
}

impl BasDec {
  /// Returns a sequence of decs.
  ///
  /// # Panics
  ///
  /// If there was an internal error.
  #[must_use]
  pub fn seq(mut decs: Vec<Self>) -> Self {
    if decs.len() == 1 {
      decs.pop().unwrap()
    } else {
      Self::Seq(decs)
    }
  }
}

/// A basis expression.
#[derive(Debug)]
pub enum BasExp {
  /// `bas <dec> end`
  Bas(BasDec),
  /// `Foo`, etc.
  Name(WithRange<str_util::Name>),
  /// `let <dec> in <exp> end`
  Let(BasDec, Box<BasExp>),
}

/// A kind of path.
#[derive(Debug, Clone, Copy)]
pub enum PathKind {
  /// An SML source path.
  Source,
  /// A group path, like MLB or CM.
  Group,
}
