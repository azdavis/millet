//! Environments and types that contain them.

#![allow(clippy::module_name_repetitions)]

use crate::info::{TyEnv, ValEnv};
use crate::ty::Ty;
use crate::{def, disallow::Disallow, sym::Sym};
use chain_map::ChainMap;
use fast_hash::{FxHashMap, FxHashSet};

/// Definition: `StrEnv`
pub type StrEnv = ChainMap<str_util::Name, Env>;

/// Definition: Env
#[derive(Debug, Default, Clone)]
pub struct Env {
  /// The structure env.
  pub str_env: StrEnv,
  /// The type env.
  pub ty_env: TyEnv,
  /// The value env.
  pub val_env: ValEnv,
  /// The definitions.
  pub defs: def::Set,
  /// Whether everything in this env is disallowed.
  pub disallow: Option<Disallow>,
}

impl Env {
  /// Returns an empty `Env` with the given defs.
  #[must_use]
  pub fn new(defs: def::Set) -> Env {
    Env { defs, ..Env::default() }
  }

  /// Appends other onto self, emptying other.
  pub fn append(&mut self, other: &mut Self) {
    self.str_env.append(&mut other.str_env);
    self.ty_env.append(&mut other.ty_env);
    self.val_env.append(&mut other.val_env);
  }

  /// Consolidates the env, making it use less memory.
  pub fn consolidate(&mut self) {
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
pub struct Cx {
  /// The env.
  pub env: Env,
  /// the Definition has this as a set, but we have it as a mapping.
  ///
  /// this isn't really `ty_vars(C)` as in the definition, since it's just fixed ty vars.
  pub fixed: FxHashMap<sml_hir::TyVar, Ty>,
}

/// Definition: `TyNameSet`
pub type TyNameSet = FxHashSet<Sym>;

/// A signature.
///
/// Definition: `Sig`
#[derive(Debug, Clone)]
pub struct Sig {
  /// The set of type names bound by this sig. For instance, `t` is bound in `sig type t end`.
  pub ty_names: TyNameSet,
  /// The env for this sig.
  pub env: Env,
  /// Whether this sig is disallowed.
  pub disallow: Option<Disallow>,
}

/// A functor signature.
///
/// Definition: `FunSig`
#[derive(Debug, Clone)]
pub struct FunSig {
  /// The sig of the parameter.
  pub param: Sig,
  /// The type names bound in the body.
  pub body_ty_names: TyNameSet,
  /// The env of the functor body.
  pub body_env: Env,
  /// What kind of functor sugar flavor this is.
  pub flavor: sml_hir::Flavor,
  /// Whether this is disallowed.
  pub disallow: Option<Disallow>,
}

/// A mapping from names to signatures.
pub type SigEnv = ChainMap<str_util::Name, Sig>;

/// A mapping from names to functor signatures.
pub type FunEnv = ChainMap<str_util::Name, FunSig>;
