//! Environments and types that contain them.

use crate::types::{FixedTyVar, Sym, TyEnv, ValEnv};
use crate::{def, disallow::Disallow};
use fast_hash::{FxHashMap, FxHashSet};
use stack_map::StackMap;

/// Definition: `StrEnv`
pub(crate) type StrEnv = StackMap<str_util::Name, Env>;

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
  pub(crate) fixed: FxHashMap<sml_hir::TyVar, FixedTyVar>,
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

pub(crate) type SigEnv = StackMap<str_util::Name, Sig>;
pub(crate) type FunEnv = StackMap<str_util::Name, FunSig>;

/// Definition: Basis
#[derive(Debug, Default, Clone)]
pub(crate) struct Bs {
  pub(crate) env: Env,
  pub(crate) sig_env: SigEnv,
  pub(crate) fun_env: FunEnv,
}

impl Bs {
  pub(crate) fn as_cx(&self) -> Cx {
    Cx { env: self.env.clone(), fixed: FxHashMap::default() }
  }

  pub(crate) fn append(&mut self, mut other: Bs) {
    self.env.append(&mut other.env);
    self.sig_env.append(&mut other.sig_env);
    self.fun_env.append(&mut other.fun_env);
  }
}
