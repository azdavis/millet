//! Checks whether an environment matches a signature.
//!
//! As noted by the Definition, when matching a signature against an environment, the type functions
//! from the environment that correspond to the bound type names in the signature are used as the
//! range of the type realization, with the type names themselves being the domain.
//!
//! Type names in Millet are symbols (`Sym`s). Symbols are essentially globally unique identifiers -
//! that is, what a symbol semantically "means" (i.e. what it refers to) does not change as a result
//! of definitions coming into or going out of scope. This distinguishes it from e.g. `StrRef`.
//!
//! To implement signature matching, we must substitute the symbols bound by the signature for the
//! corresponding symbols with the same name bound by the environment which we are matching against
//! the signature, in the context of the environment of the signature. (Note that a signature is
//! essentially a 2-tuple of a set of bound type names and and environment. Note also that when we
//! say "the same name", we mean with respect to the `StrRef` inside every `Sym`.)
//!
//! To do this, we construct a `SymSubst`, which essentially acts as the type realization, and pass
//! that down to `enrich::ck`, which applied the `SymSubst` when appropriate, that is, right before
//! we are about to check if two types unify.
//!
//! This approach is mildly unfortunate, since enrichment checking is not supposed to be concerned
//! with type realizations. Is this what is called a 'leaky abstraction'? Anyway, look at
//! `enrich.rs` for more commentary.

use crate::loc::Loc;
use crate::statics::ck::enrich;
use crate::statics::ck::util::get_ty_sym;
use crate::statics::types::{Env, Result, Sig, State, SymSubst, TyEnv};

/// Returns `Ok(E)` iff `sig >= E` and `env >> E`. TODO pass by value?
pub fn ck(st: &mut State, loc: Loc, env: &Env, sig: &Sig) -> Result<Env> {
  let mut sym_subst = SymSubst::default();
  for &sig_ty_sym in sig.ty_names.iter() {
    let ty_name = loc.wrap(sig_ty_sym.name());
    let env_ty_sym = get_ty_sym(env, ty_name)?;
    sym_subst.insert(sig_ty_sym, env_ty_sym);
  }
  enrich::ck(loc, &st.tys, &sym_subst, env, &sig.env)?;
  // TODO is this right? what about opaque ascription?
  Ok(Env {
    str_env: env
      .str_env
      .iter()
      .filter(|(name, _)| sig.env.str_env.contains_key(name))
      .map(clone_pair)
      .collect(),
    ty_env: TyEnv {
      inner: env
        .ty_env
        .inner
        .iter()
        .filter(|(name, _)| sig.env.ty_env.inner.contains_key(name))
        .map(clone_pair)
        .collect(),
    },
    val_env: env
      .val_env
      .iter()
      .filter(|(name, _)| sig.env.val_env.contains_key(name))
      .map(clone_pair)
      .collect(),
  })
}

fn clone_pair<T: Clone, U: Clone>((x, y): (&T, &U)) -> (T, U) {
  (x.clone(), y.clone())
}
