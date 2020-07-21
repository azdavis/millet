//! Checks whether an environment matches a signature.

use crate::loc::Loc;
use crate::statics::ck::enrich;
use crate::statics::types::{Env, Result, Sig, State, SymSubst};

/// Returns `Ok(E)` iff `sig >= E` and `env >> E`. TODO pass by value?
pub fn ck(st: &mut State, loc: Loc, env: &Env, sig: &Sig) -> Result<Env> {
  let mut sym_subst = SymSubst::default();
  for &old in sig.ty_names.iter() {
    let ty_info = st.tys.get(&old).clone();
    let new = st.new_sym(loc.wrap(old.name()));
    sym_subst.insert(old, new);
    st.tys.insert(new, ty_info);
  }
  let mut sig_env = sig.env.clone();
  sig_env.apply_sym_subst(&sym_subst);
  enrich::ck(loc, &st.tys, env, &sig_env)?;
  Ok(sig_env)
}
