//! Type realizations.

use crate::types::ty::{Ty, TyData, TyScheme, Tys};
use crate::types::util::apply_bv;
use crate::{core_info::ValEnv, env::Env, sym::Sym};
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

pub(crate) fn get_env(tys: &mut Tys, subst: &TyRealization, env: &mut Env) {
  for (_, env) in env.str_env.iter_mut() {
    get_env(tys, subst, env);
  }
  for (_, ty_info) in env.ty_env.iter_mut() {
    get_ty(tys, subst, &mut ty_info.ty_scheme.ty);
    get_val_env(tys, subst, &mut ty_info.val_env);
  }
  get_val_env(tys, subst, &mut env.val_env);
}

pub(crate) fn get_val_env(tys: &mut Tys, subst: &TyRealization, val_env: &mut ValEnv) {
  for (_, val_info) in val_env.iter_mut() {
    get_ty(tys, subst, &mut val_info.ty_scheme.ty);
  }
}

fn get_ty(tys: &mut Tys, subst: &TyRealization, ty: &mut Ty) {
  match tys.data(*ty) {
    // interesting case
    TyData::Con(mut data) => {
      for ty in &mut data.args {
        get_ty(tys, subst, ty);
      }
      match subst.0.get(&data.sym) {
        None => *ty = tys.con(data.args, data.sym),
        Some(ty_scheme) => {
          if data.args.len() == ty_scheme.bound_vars.len() {
            *ty = ty_scheme.ty;
            apply_bv(tys, &data.args, ty);
          } else if cfg!(debug_assertions) {
            // not sure if this is actually reachable given how we construct the `TyRealization` and
            // how we've checked everything that were now applying the realization to.
            unreachable!("malformed TyRealization");
          }
        }
      }
    }
    // trivial base cases
    TyData::None | TyData::BoundVar(_) | TyData::UnsolvedMetaVar(_) | TyData::FixedVar(_) => {}
    // recursive cases
    TyData::Record(mut rows) => {
      for ty in rows.values_mut() {
        get_ty(tys, subst, ty);
      }
      *ty = tys.record(rows);
    }
    TyData::Fn(mut data) => {
      get_ty(tys, subst, &mut data.param);
      get_ty(tys, subst, &mut data.res);
      *ty = tys.fun(data.param, data.res);
    }
  }
}
