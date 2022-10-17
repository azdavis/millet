//! Determining the order in which to analyze a set of paths that will minimize undefined
//! structure-level name errors.

use crate::basis::Basis;
use crate::error::{ErrorKind, Item};
use crate::info::Mode;
use crate::st::St;
use crate::top_dec;
use crate::types::{Bs, Env, EnvLike as _, EnvStack, FunEnv, SigEnv, Syms};

/// An unordered map from paths to HIR ready for analysis.
pub type SmlHirPaths<'a> = paths::PathMap<(&'a sml_hir::Arenas, sml_hir::StrDecIdx)>;

/// Get the ordering.
#[must_use]
pub fn get(mut syms: Syms, mut basis: Basis, mut paths: SmlHirPaths<'_>) -> Vec<paths::PathId> {
  let mut ok_paths = Vec::<paths::PathId>::new();
  basis = {
    let mut env = basis.inner.env.into_env();
    let mut sig_env = basis.inner.sig_env.as_ref().clone();
    let mut fun_env = basis.inner.fun_env.as_ref().clone();
    for &(arenas, root) in paths.values() {
      rm_top_level_defs(&mut env, &mut sig_env, &mut fun_env, arenas, root);
    }
    Basis {
      inner: Bs {
        env: EnvStack::one(env),
        fun_env: std::sync::Arc::new(fun_env),
        sig_env: std::sync::Arc::new(sig_env),
      },
    }
  };
  loop {
    let old_ok_paths_len = ok_paths.len();
    let mut new_paths: SmlHirPaths<'_> = fast_hash::map_with_capacity(paths.len());
    for (path, (arenas, root)) in paths {
      // TODO greatly limit the checks we do in statics since much of them are useless for getting
      // the path order.
      let mut st = St::new(Mode::Regular(Some(path)), syms);
      let inner = top_dec::get(&mut st, &basis.inner, arenas, root);
      let (new_syms, errors, _) = st.finish();
      syms = new_syms;
      let any_needed = errors.iter().any(|x| {
        matches!(x.kind, ErrorKind::Undefined(Item::Functor | Item::Sig | Item::Struct, _))
      });
      if any_needed {
        new_paths.insert(path, (arenas, root));
      } else {
        basis.append(Basis { inner });
        ok_paths.push(path);
      }
    }
    paths = new_paths;
    let failed_to_advance = old_ok_paths_len == ok_paths.len();
    let nothing_left = paths.is_empty();
    if failed_to_advance || nothing_left {
      // if `failed_to_advance`, we failed to get any newly ok paths. that means there is no
      // ordering of all the paths that causes them all to have no undefined structure-level name
      // errors. so come up with a fake ordering: first the ordering we could figure out, then all
      // the other paths that we couldn't figure out, in an arbitrary but stable order.
      //
      // if `nothing_left`, we're done since we successfully processed all the paths. then the
      // ordering is exactly `ok_paths`, and `paths` is empty.
      let mut rest: Vec<_> = paths.into_keys().collect();
      rest.sort_unstable();
      ok_paths.append(&mut rest);
      return ok_paths;
    }
  }
}

fn rm_top_level_defs(
  env: &mut Env,
  sig_env: &mut SigEnv,
  fun_env: &mut FunEnv,
  ars: &sml_hir::Arenas,
  dec: sml_hir::StrDecIdx,
) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[dec] {
    sml_hir::StrDec::Dec(_) => {}
    sml_hir::StrDec::Structure(binds) => {
      for bind in binds {
        env.str_env.remove(&bind.name);
      }
    }
    sml_hir::StrDec::Signature(binds) => {
      for bind in binds {
        sig_env.remove(&bind.name);
      }
    }
    sml_hir::StrDec::Functor(binds) => {
      for bind in binds {
        fun_env.remove(&bind.functor_name);
      }
    }
    sml_hir::StrDec::Local(_, in_dec) => rm_top_level_defs(env, sig_env, fun_env, ars, *in_dec),
    sml_hir::StrDec::Seq(decs) => {
      for &dec in decs {
        rm_top_level_defs(env, sig_env, fun_env, ars, dec);
      }
    }
  }
}
