//! Environments and types that contain them.

use crate::core_info::{TyEnv, ValEnv};
use crate::types::ty::Ty;
use crate::{def, disallow::Disallow, sym::Sym};
use chain_map::ChainMap;
use fast_hash::{FxHashMap, FxHashSet};

/// Definition: `StrEnv`
pub(crate) type StrEnv = ChainMap<str_util::Name, Env>;

/// Definition: Env
#[derive(Debug, Default, Clone)]
pub(crate) struct Env {
  pub(crate) str_env: StrEnv,
  pub(crate) ty_env: TyEnv,
  pub(crate) val_env: ValEnv,
  pub(crate) def: Option<def::Def>,
  pub(crate) disallow: Option<Disallow>,
}

impl Env {
  pub(crate) fn with_def(def: Option<def::Def>) -> Self {
    Self { def, ..Default::default() }
  }

  pub(crate) fn append(&mut self, other: &mut Self) {
    self.str_env.append(&mut other.str_env);
    self.ty_env.append(&mut other.ty_env);
    self.val_env.append(&mut other.val_env);
  }

  pub(crate) fn consolidate(&mut self) {
    self.str_env.consolidate();
    self.ty_env.consolidate();
    self.val_env.consolidate();
  }
}

/// Definition: Context
///
/// No need for the set of ty names as from the Definition; it seems to only be used to ensure a
/// type name does not escape its scope, and for that we use `Sym::generated_after`.
#[derive(Debug, Clone)]
pub(crate) struct Cx {
  pub(crate) env: Env,
  /// the Definition has this as a set, but we have it as a mapping.
  ///
  /// this isn't really `ty_vars(C)` as in the definition, since it's just fixed ty vars.
  pub(crate) fixed: FxHashMap<sml_hir::TyVar, Ty>,
}

/// Definition: `TyNameSet`
pub(crate) type TyNameSet = FxHashSet<Sym>;

/// Definition: Sig
#[derive(Debug, Clone)]
pub(crate) struct Sig {
  pub(crate) ty_names: TyNameSet,
  pub(crate) env: Env,
  pub(crate) disallow: Option<Disallow>,
}

/// Definition: `FunSig`
#[derive(Debug, Clone)]
pub(crate) struct FunSig {
  pub(crate) param: Sig,
  pub(crate) body_ty_names: TyNameSet,
  pub(crate) body_env: Env,
  pub(crate) flavor: sml_hir::Flavor,
  pub(crate) disallow: Option<Disallow>,
}

pub(crate) type SigEnv = ChainMap<str_util::Name, Sig>;
pub(crate) type FunEnv = ChainMap<str_util::Name, FunSig>;
