//! Environments and types that contain them.

use crate::types::{FixedTyVar, Sym, TyEnv, TyInfo, ValEnv, ValInfo};
use crate::{def, disallow::Disallow};
use fast_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;

/// An environment-like type.
pub(crate) trait EnvLike {
  fn get_str(&self, name: &str_util::Name) -> Option<&Env>;
  fn get_ty(&self, name: &str_util::Name) -> Option<&TyInfo>;
  fn get_val(&self, name: &str_util::Name) -> Option<&ValInfo>;
  /// empties other into self.
  fn append(&mut self, other: &mut Env);
  fn all_str(&self) -> FxHashMap<&str_util::Name, &Env>;
  fn all_ty(&self) -> FxHashMap<&str_util::Name, &TyInfo>;
  fn all_val(&self) -> FxHashMap<&str_util::Name, &ValInfo>;
  fn into_env(self) -> Env;
}

/// Definition: `StrEnv`
pub(crate) type StrEnv = FxHashMap<str_util::Name, Env>;

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
}

impl EnvLike for Env {
  fn get_str(&self, name: &str_util::Name) -> Option<&Env> {
    self.str_env.get(name)
  }

  fn get_ty(&self, name: &str_util::Name) -> Option<&TyInfo> {
    self.ty_env.get(name)
  }

  fn get_val(&self, name: &str_util::Name) -> Option<&ValInfo> {
    self.val_env.get(name)
  }

  fn append(&mut self, other: &mut Self) {
    self.str_env.extend(other.str_env.drain());
    self.ty_env.extend(other.ty_env.drain());
    self.val_env.extend(other.val_env.drain());
  }

  fn all_str(&self) -> FxHashMap<&str_util::Name, &Env> {
    self.str_env.iter().collect()
  }

  fn all_ty(&self) -> FxHashMap<&str_util::Name, &TyInfo> {
    self.ty_env.iter().collect()
  }

  fn all_val(&self) -> FxHashMap<&str_util::Name, &ValInfo> {
    self.val_env.iter().collect()
  }

  fn into_env(self) -> Env {
    self
  }
}

/// A wrapper around a stack of [`Env`]s. Acts like a single `Env` in most respects, but is faster
/// to `Clone`.
#[derive(Debug, Default, Clone)]
pub(crate) struct EnvStack {
  rest: Vec<Arc<Env>>,
  top: Env,
}

impl EnvStack {
  pub(crate) fn one(env: Env) -> Self {
    Self { rest: Vec::new(), top: env }
  }

  pub(crate) fn push(&mut self, other: Env) {
    let old_top = std::mem::replace(&mut self.top, other);
    self.rest.push(Arc::new(old_top));
  }

  pub(crate) fn as_one_mut(&mut self) -> &mut Env {
    assert!(self.rest.is_empty());
    &mut self.top
  }

  fn all_env(&self) -> impl DoubleEndedIterator<Item = &Env> + '_ {
    self.rest.iter().map(AsRef::as_ref).chain(std::iter::once(&self.top))
  }

  fn all_env_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Env> + '_ {
    self.rest.iter_mut().map(Arc::make_mut).chain(std::iter::once(&mut self.top))
  }
}

impl EnvLike for EnvStack {
  fn get_str(&self, name: &str_util::Name) -> Option<&Env> {
    self.all_env().rev().find_map(|env| env.str_env.get(name))
  }

  fn get_ty(&self, name: &str_util::Name) -> Option<&TyInfo> {
    self.all_env().rev().find_map(|env| env.ty_env.get(name))
  }

  fn get_val(&self, name: &str_util::Name) -> Option<&ValInfo> {
    self.all_env().rev().find_map(|env| env.val_env.get(name))
  }

  fn append(&mut self, other: &mut Env) {
    let mut env = Env::default();
    env.append(other);
    self.push(env);
  }

  fn all_str(&self) -> FxHashMap<&str_util::Name, &Env> {
    let mut ret = FxHashMap::<&str_util::Name, &Env>::default();
    for env in self.all_env() {
      ret.extend(env.str_env.iter());
    }
    ret
  }

  fn all_ty(&self) -> FxHashMap<&str_util::Name, &TyInfo> {
    let mut ret = FxHashMap::<&str_util::Name, &TyInfo>::default();
    for env in self.all_env() {
      ret.extend(env.ty_env.iter());
    }
    ret
  }

  fn all_val(&self) -> FxHashMap<&str_util::Name, &ValInfo> {
    let mut ret = FxHashMap::<&str_util::Name, &ValInfo>::default();
    for env in self.all_env() {
      ret.extend(env.val_env.iter());
    }
    ret
  }

  fn into_env(mut self) -> Env {
    let mut env = Env::default();
    for other in self.all_env_mut() {
      env.append(other);
    }
    env
  }
}

/// Definition: Context
///
/// No need for the set of ty names as from the Definition; it seems to only be used to ensure a
/// type name does not escape its scope, and for that we use `Sym::generated_after`.
#[derive(Debug, Clone)]
pub(crate) struct Cx {
  pub(crate) env: EnvStack,
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

pub(crate) type SigEnv = FxHashMap<str_util::Name, Sig>;
pub(crate) type FunEnv = FxHashMap<str_util::Name, FunSig>;

/// Definition: Basis
#[derive(Debug, Default, Clone)]
pub(crate) struct Bs {
  pub(crate) env: EnvStack,
  pub(crate) sig_env: Arc<SigEnv>,
  pub(crate) fun_env: Arc<FunEnv>,
}

impl Bs {
  pub(crate) fn as_cx(&self) -> Cx {
    Cx { env: self.env.clone(), fixed: FxHashMap::default() }
  }

  pub(crate) fn as_mut_fun_env(&mut self) -> &mut FunEnv {
    Arc::make_mut(&mut self.fun_env)
  }

  pub(crate) fn as_mut_sig_env(&mut self) -> &mut SigEnv {
    Arc::make_mut(&mut self.sig_env)
  }

  pub(crate) fn append(&mut self, mut other: Bs) {
    self.as_mut_sig_env().extend(other.as_mut_sig_env().drain());
    self.as_mut_fun_env().extend(other.as_mut_fun_env().drain());
    self.env.append(&mut other.env.into_env());
  }
}
