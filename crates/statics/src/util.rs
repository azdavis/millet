use crate::error::Error;
use crate::st::{St, Subst};
use crate::types::{Env, Sym, Ty, TyScheme};
use std::collections::BTreeMap;

pub(crate) fn get_scon(scon: &hir::SCon) -> Ty {
  let sym = match scon {
    hir::SCon::Int(_) => Sym::INT,
    hir::SCon::Real(_) => Sym::REAL,
    hir::SCon::Word(_) => Sym::WORD,
    hir::SCon::Char(_) => Sym::CHAR,
    hir::SCon::String(_) => Sym::STRING,
  };
  Ty::zero(sym)
}

pub(crate) fn record<T, F>(st: &mut St, rows: &[(hir::Lab, T)], mut f: F) -> Ty
where
  T: Copy,
  F: FnMut(&mut St, &hir::Lab, T) -> Ty,
{
  let mut ty_rows = BTreeMap::<hir::Lab, Ty>::new();
  for (lab, val) in rows {
    let ty = f(st, lab, *val);
    match ty_rows.insert(lab.clone(), ty) {
      None => {}
      Some(_) => st.err(Error::DuplicateLab(lab.clone())),
    }
  }
  Ty::Record(ty_rows)
}

pub(crate) fn get_env<'e, 'p>(
  mut env: &'e Env,
  path: &'p hir::Path,
) -> Result<&'e Env, &'p hir::Name> {
  for name in path.structures() {
    match env.str_env.get(name) {
      None => return Err(name),
      Some(x) => env = x,
    }
  }
  Ok(env)
}

/// substitute any meta type variables in `ty` with their types in `subst`.
///
/// meta variables not defined by the `subst` are left alone.
pub(crate) fn apply(subst: &Subst, ty: &mut Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(mv) {
      None => {}
      Some(t) => {
        let mut t = t.clone();
        apply(subst, &mut t);
        *ty = t;
      }
    },
    Ty::Record(rows) => {
      for ty in rows.values_mut() {
        apply(subst, ty);
      }
    }
    Ty::Con(args, _) => {
      for ty in args.iter_mut() {
        apply(subst, ty);
      }
    }
    Ty::Fn(param, res) => {
      apply(subst, param);
      apply(subst, res);
    }
  }
}

pub(crate) fn instantiate(st: &mut St, ty_scheme: &TyScheme) -> Ty {
  let meta_vars: Vec<_> = st
    .gen_from_ty_vars(&ty_scheme.vars)
    .map(Ty::MetaVar)
    .collect();
  let mut ty = ty_scheme.ty.clone();
  apply_bv(&meta_vars, &mut ty);
  ty
}

/// like [`apply`], but for bound type variables.
///
/// unlike `apply`, all bound variables must be defined by the subst.
pub(crate) fn apply_bv(subst: &[Ty], ty: &mut Ty) {
  match ty {
    Ty::None | Ty::MetaVar(_) => {}
    Ty::BoundVar(bv) => *ty = bv.index_into(subst).clone(),
    Ty::Record(rows) => {
      for ty in rows.values_mut() {
        apply_bv(subst, ty);
      }
    }
    Ty::Con(args, _) => {
      for ty in args.iter_mut() {
        apply_bv(subst, ty);
      }
    }
    Ty::Fn(param, res) => {
      apply_bv(subst, param);
      apply_bv(subst, res);
    }
  }
}
