//! Information about SML Core constructs.

use crate::sym::Exn;
use crate::types::ty::TyScheme;
use crate::{def, disallow::Disallow};
use chain_map::ChainMap;
use fast_hash::FxHashSet;

/// Definition: `TyStr`
#[derive(Debug, Clone)]
pub(crate) struct TyInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) val_env: ValEnv,
  pub(crate) def: Option<def::Def>,
  pub(crate) disallow: Option<Disallow>,
}

/// Definition: `TyEnv`
pub(crate) type TyEnv = ChainMap<str_util::Name, TyInfo>;

/// Definition: `ValEnv`
pub(crate) type ValEnv = ChainMap<str_util::Name, ValInfo>;

#[derive(Debug, Clone)]
pub(crate) struct ValInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) id_status: IdStatus,
  /// a set, because we can have or patterns
  pub(crate) defs: FxHashSet<def::Def>,
  pub(crate) disallow: Option<Disallow>,
}

/// Definition: `IdStatus`
#[derive(Debug, Clone, Copy)]
pub(crate) enum IdStatus {
  Con,
  Exn(Exn),
  Val,
}

impl IdStatus {
  pub(crate) fn same_kind_as(self, other: Self) -> bool {
    matches!(
      (self, other),
      (Self::Con, Self::Con) | (Self::Exn(_), Self::Exn(_)) | (Self::Val, Self::Val)
    )
  }
}
