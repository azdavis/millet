//! Type realizations.

use crate::types::{Sym, Ty, TyScheme, ValEnv};
use crate::{env::Env, util::apply_bv};
use fast_hash::FxHashMap;

/// A type realization.
#[derive(Debug, Default)]
pub(crate) struct TyRealization(FxHashMap<Sym, TyScheme>);

impl TyRealization {
  pub(crate) fn clear(&mut self) {
    self.0.clear();
  }

  /// Inserts the mapping from `sym` to `ty_scheme`.
  ///
  /// Callers **must** ensure `sym` has the same arity as `ty_scheme`.
  ///
  /// Panics if this overwrites an existing `Sym`.
  pub(crate) fn insert(&mut self, sym: Sym, ty_scheme: TyScheme) {
    assert!(self.0.insert(sym, ty_scheme).is_none());
  }
}

pub(crate) fn get_env(subst: &TyRealization, env: &mut Env) {
  for env in env.str_env.values_mut() {
    get_env(subst, env);
  }
  for ty_info in env.ty_env.values_mut() {
    get_ty(subst, &mut ty_info.ty_scheme.ty);
    get_val_env(subst, &mut ty_info.val_env);
  }
  get_val_env(subst, &mut env.val_env);
}

pub(crate) fn get_val_env(subst: &TyRealization, val_env: &mut ValEnv) {
  for val_info in val_env.values_mut() {
    get_ty(subst, &mut val_info.ty_scheme.ty);
  }
}

fn get_ty(subst: &TyRealization, ty: &mut Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values_mut() {
        get_ty(subst, ty);
      }
    }
    Ty::Con(args, sym) => {
      for ty in args.iter_mut() {
        get_ty(subst, ty);
      }
      if let Some(ty_scheme) = subst.0.get(sym) {
        if args.len() == ty_scheme.bound_vars.len() {
          let mut ty_scheme_ty = ty_scheme.ty.clone();
          apply_bv(args, &mut ty_scheme_ty);
          *ty = ty_scheme_ty;
        } else if cfg!(debug_assertions) {
          // not sure if this is actually reachable given how we construct the `TyRealization` and
          // how we've checked everything that were now applying the realization to.
          unreachable!("malformed TyRealization");
        }
      }
    }
    Ty::Fn(param, res) => {
      get_ty(subst, param);
      get_ty(subst, res);
    }
  }
}
