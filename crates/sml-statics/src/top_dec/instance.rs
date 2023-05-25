//! Signature instantiation.

use crate::error::ErrorKind;
use crate::{compatible::eq_ty_fn_no_emit, get_env::get_ty_info_raw, st::St, top_dec::realize};
use sml_statics_types::env::{Env, Sig};
use sml_statics_types::{ty::TyScheme, util::n_ary_con};

pub(crate) fn env_of_sig(
  st: &mut St,
  idx: sml_hir::Idx,
  subst: &mut realize::TyRealization,
  env: &Env,
  sig: &Sig,
) {
  for &sym in &sig.ty_names {
    let mut path = Vec::<&str_util::Name>::new();
    let bound_vars = st.syms_tys.syms.get(sym).unwrap().ty_info.ty_scheme.bound_vars.clone();
    let ty_scheme = n_ary_con(&mut st.syms_tys.tys, bound_vars, sym);
    if !bound_ty_name_to_path(st, &mut path, &sig.env, &ty_scheme) {
      // @test(sig::no_path_to_sym). there should have already been an error emitted for this
      log::warn!("no path to sym");
      return;
    }
    let last = path.pop().unwrap();
    let want = ty_scheme.bound_vars.len();
    let ty_info = get_ty_info_raw(env, path, last);
    for e in ty_info.disallow {
      st.err(idx, e.into());
    }
    match ty_info.val {
      Ok(ty_info) => {
        let got = ty_info.ty_scheme.bound_vars.len();
        if want == got {
          subst.insert(sym, ty_info.ty_scheme.clone());
        } else {
          st.err(idx, ErrorKind::WrongNumTyArgs(want, got));
        }
      }
      Err(e) => st.err(idx, e.into()),
    }
  }
}

/// note that given an environment for the signature:
///
/// ```sml
/// signature SIG = sig
///   type t
///   type u = t
/// end
/// ```
///
/// and a request to find the path to the single ty name bound by this signature's env (there is
/// only one), this function will report _either_ `t` or `u` based on which one comes up first in
/// the `iter()` order.
///
/// this seems slightly questionable, but I'm not actually sure if it's an issue. I mean, it says
/// right there that they should be equal anyway.
fn bound_ty_name_to_path<'e>(
  st: &mut St,
  ac: &mut Vec<&'e str_util::Name>,
  env: &'e Env,
  ty_scheme: &TyScheme,
) -> bool {
  for (name, ty_info) in env.ty_env.iter() {
    if eq_ty_fn_no_emit(st, ty_info.ty_scheme.clone(), ty_scheme.clone()).is_ok() {
      ac.push(name);
      return true;
    }
  }
  for (name, env) in env.str_env.iter() {
    ac.push(name);
    if bound_ty_name_to_path(st, ac, env, ty_scheme) {
      return true;
    }
    ac.pop();
  }
  false
}
