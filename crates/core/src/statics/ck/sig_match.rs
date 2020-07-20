//! Checks whether an environment matches a signature.

// TODO remove
#![allow(unused)]

use crate::loc::Loc;
use crate::statics::ck::enrich;
use crate::statics::types::{Env, Result, Sig, State, SymSubst, Tys};
use std::collections::HashMap;

/// Returns `Ok(())` iff `env` matches `sig`.
pub fn ck(st: &mut State, loc: Loc, env: &Env, sig: &Sig) -> Result<()> {
  let mut sym_subst = SymSubst::default();
  for &old in sig.ty_names.iter() {
    let ty_info = st.tys.get(&old).clone();
    let new = st.new_sym(loc.wrap(old.name()));
    sym_subst.insert(old, new);
    st.tys.insert(new, ty_info);
  }
  let mut sig_env = sig.env.clone();
  sig_env.apply_sym_subst(&sym_subst);
  enrich::ck(loc, &st.tys, env, &sig_env)
}
