//! Fixed type variables, as written in code.

use std::fmt;
use uniq::{Uniq, UniqGen};

/// Corresponds to a user written type variable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FixedTyVar {
  id: Uniq,
  ty_var: sml_hir::TyVar,
  src: TyVarSrc,
}

impl FixedTyVar {
  pub(crate) fn ty_var(&self) -> &sml_hir::TyVar {
    &self.ty_var
  }

  pub(crate) fn src(&self) -> TyVarSrc {
    self.src
  }
}

impl fmt::Display for FixedTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ty_var.fmt(f)
  }
}

/// Where a type variable was bound.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum TyVarSrc {
  /// Bound at `type` or `datatype` (or `where type`).
  Ty,
  /// Bound at `val` (or `fun`).
  Val,
}

#[derive(Debug, Default)]
pub(crate) struct FixedTyVarGen(UniqGen);

impl FixedTyVarGen {
  pub(crate) fn gen(&mut self, ty_var: sml_hir::TyVar, src: TyVarSrc) -> FixedTyVar {
    FixedTyVar { id: self.0.gen(), ty_var, src }
  }
}
