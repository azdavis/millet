//! Traversing the `Sym`s in an `EnvLike`.

use crate::types::{Sym, ValEnv};
use crate::{env::EnvLike, util::ty_syms};

/// Calls `f` for every sym in the env.
///
/// Putting `f` last allows calls with the closure constructed at the call site to be formatted
/// across fewer lines.
pub(crate) fn get<E, F>(env: &E, f: &mut F)
where
  E: EnvLike,
  F: FnMut(Sym),
{
  for (_, env) in env.all_str() {
    get(env, f);
  }
  for (_, ty_info) in env.all_ty() {
    ty_syms(&ty_info.ty_scheme.ty, f);
    val_env_syms(&ty_info.val_env, f);
  }
  for (_, val_info) in env.all_val() {
    ty_syms(&val_info.ty_scheme.ty, f);
  }
}

fn val_env_syms<F: FnMut(Sym)>(val_env: &ValEnv, f: &mut F) {
  for val_info in val_env.values() {
    ty_syms(&val_info.ty_scheme.ty, f);
  }
}
