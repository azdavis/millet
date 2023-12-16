//! Information about SML Core constructs.

#![allow(clippy::module_name_repetitions)]

use crate::sym::Exn;
use crate::ty::TyScheme;
use crate::{def, disallow::Disallow};
use chain_map::ChainMap;

/// Information about a type.
///
/// Definition: `TyStr`
#[derive(Debug, Clone)]
pub struct TyInfo<VE = ValEnv> {
  /// The type scheme.
  pub ty_scheme: TyScheme,
  /// The val environment.
  pub val_env: VE,
  /// The definitions.
  pub defs: def::Set,
  /// Whether this is disallowed.
  pub disallow: Option<Disallow>,
}

impl<VE> TyInfo<VE>
where
  VE: Into<ValEnv>,
{
  /// Returns this with the default type of val env.
  pub fn with_default_val_env_type(self) -> TyInfo<ValEnv> {
    TyInfo {
      ty_scheme: self.ty_scheme,
      val_env: self.val_env.into(),
      defs: self.defs,
      disallow: self.disallow,
    }
  }
}

/// Definition: `TyEnv`
pub type TyEnv = ChainMap<str_util::Name, TyInfo>;

/// Definition: `ValEnv`
pub type ValEnv = ChainMap<str_util::Name, ValInfo>;

/// Information about a value.
#[derive(Debug, Clone)]
pub struct ValInfo {
  /// The type scheme for this value.
  pub ty_scheme: TyScheme,
  /// The identifier status.
  pub id_status: IdStatus,
  /// The definitions.
  ///
  /// Note that for or patterns, each occurrence of a variable in each or pattern alternative should be one of
  ///   the defs.
  /// - Structures ascribing to signatures. The definition in the structure and signature are both
  ///   important.
  ///
  /// It's an ordered set because we want to show documentation for all the defs in a stable order.
  pub defs: def::Set,
  /// Whether this is disallowed.
  pub disallow: Option<Disallow>,
}

/// An identifier status, denoting what kind of value this is.
///
/// Definition: `IdStatus`
#[derive(Debug, Clone, Copy)]
pub enum IdStatus {
  /// A constructor.
  Con,
  /// An exception.
  Exn(Exn),
  /// A general value.
  Val,
}

impl IdStatus {
  /// Returns whether this is the same kind of identifier status as the other.
  #[must_use]
  pub fn same_kind_as(self, other: Self) -> bool {
    matches!(
      (self, other),
      (Self::Con, Self::Con) | (Self::Exn(_), Self::Exn(_)) | (Self::Val, Self::Val)
    )
  }
}

/// A map from an arena index to id status.
pub type IdStatusMap<T> = sml_hir::la_arena::ArenaMap<sml_hir::la_arena::Idx<T>, IdStatus>;
