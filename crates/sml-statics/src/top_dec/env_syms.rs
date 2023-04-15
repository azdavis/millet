//! Traversing the `Sym`s in an `Env`.

use crate::types::{ty::Tys, util::ty_syms};
use crate::{core_info::ValEnv, env::Env, sym::Sym};

/// Calls `f` for every sym in the env.
///
/// Putting `f` last allows calls with the closure constructed at the call site to be formatted
/// across fewer lines.
pub(crate) fn get<F>(tys: &Tys, env: &Env, f: &mut F)
where
  F: FnMut(Sym),
{
  for (_, env) in env.str_env.iter() {
    get(tys, env, f);
  }
  for (_, ty_info) in env.ty_env.iter() {
    ty_syms(tys, ty_info.ty_scheme.ty, f);
    val_env_syms(tys, &ty_info.val_env, f);
  }
  for (_, val_info) in env.val_env.iter() {
    ty_syms(tys, val_info.ty_scheme.ty, f);
  }
}

fn val_env_syms<F: FnMut(Sym)>(tys: &Tys, val_env: &ValEnv, f: &mut F) {
  for (_, val_info) in val_env.iter() {
    ty_syms(tys, val_info.ty_scheme.ty, f);
  }
}
