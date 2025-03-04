//! Collect all the paths to ty cons in an env.

use crate::get_env::{GetEnvResult, get_env};
use fast_hash::FxHashSet;
use sml_statics_types::env::Env;

pub(crate) fn get(env: &Env, path: &sml_path::Path) -> GetEnvResult<FxHashSet<sml_path::Path>> {
  let got_env = get_env(env, path.all_names());
  let val = got_env.val.map(|env| {
    let mut ty_cons = FxHashSet::<sml_path::Path>::default();
    go(&mut Vec::new(), &mut ty_cons, env);
    ty_cons
  });
  GetEnvResult { val, disallow: got_env.disallow }
}

fn go(prefix: &mut Vec<str_util::Name>, ac: &mut FxHashSet<sml_path::Path>, env: &Env) {
  ac.extend(env.ty_env.iter().map(|(name, _)| sml_path::Path::new(prefix.clone(), name.clone())));
  for (name, env) in env.str_env.iter() {
    prefix.push(name.clone());
    go(prefix, ac, env);
    prefix.pop().unwrap();
  }
}

/// Joins two sequential paths into one.
pub(crate) fn join_paths(p1: &sml_path::Path, p2: &sml_path::Path) -> sml_path::Path {
  sml_path::Path::new(p1.all_names().chain(p2.prefix()).cloned(), p2.last().clone())
}
