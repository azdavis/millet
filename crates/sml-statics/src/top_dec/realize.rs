//! Type realizations.

use crate::types::{Sym, Ty, TyScheme, ValEnv};
use crate::{env::Env, error::ErrorKind, st::St, util::apply_bv};
use fast_hash::FxHashMap;

pub(crate) type TyRealization = FxHashMap<Sym, TyScheme>;

pub(crate) fn get_env(st: &mut St, idx: sml_hir::Idx, subst: &TyRealization, env: &mut Env) {
  for env in env.str_env.values_mut() {
    get_env(st, idx, subst, env);
  }
  for ty_info in env.ty_env.values_mut() {
    get_ty(st, idx, subst, &mut ty_info.ty_scheme.ty);
    get_val_env(st, idx, subst, &mut ty_info.val_env);
  }
  get_val_env(st, idx, subst, &mut env.val_env);
}

pub(crate) fn get_val_env(
  st: &mut St,
  idx: sml_hir::Idx,
  subst: &TyRealization,
  val_env: &mut ValEnv,
) {
  for val_info in val_env.values_mut() {
    get_ty(st, idx, subst, &mut val_info.ty_scheme.ty);
  }
}

fn get_ty(st: &mut St, idx: sml_hir::Idx, subst: &TyRealization, ty: &mut Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values_mut() {
        get_ty(st, idx, subst, ty);
      }
    }
    Ty::Con(args, sym) => {
      for ty in args.iter_mut() {
        get_ty(st, idx, subst, ty);
      }
      if let Some(ty_scheme) = subst.get(sym) {
        let want = args.len();
        let got = ty_scheme.bound_vars.len();
        if want == got {
          let mut ty_scheme_ty = ty_scheme.ty.clone();
          apply_bv(args, &mut ty_scheme_ty);
          *ty = ty_scheme_ty;
        } else {
          // TODO remove?
          st.err(idx, ErrorKind::WrongNumTyArgs(want, got));
        }
      }
    }
    Ty::Fn(param, res) => {
      get_ty(st, idx, subst, param);
      get_ty(st, idx, subst, res);
    }
  }
}
