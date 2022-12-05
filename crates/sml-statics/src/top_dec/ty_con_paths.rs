//! Collect all the paths to ty cons in an env.

use crate::env::{Env, EnvLike};
use crate::{error::ErrorKind, get_env::get_env_from_str_path};
use fast_hash::FxHashSet;

pub(crate) fn get<E: EnvLike>(
  env: &E,
  path: &sml_hir::Path,
) -> Result<FxHashSet<sml_hir::Path>, ErrorKind> {
  let got_env = get_env_from_str_path(env, path)?;
  let mut ty_cons = FxHashSet::<sml_hir::Path>::default();
  go(&mut Vec::new(), &mut ty_cons, got_env);
  Ok(ty_cons)
}

fn go(prefix: &mut Vec<str_util::Name>, ac: &mut FxHashSet<sml_hir::Path>, env: &Env) {
  ac.extend(env.ty_env.keys().map(|name| sml_hir::Path::new(prefix.clone(), name.clone())));
  for (name, env) in &env.str_env {
    prefix.push(name.clone());
    go(prefix, ac, env);
    prefix.pop().unwrap();
  }
}
