use crate::cx::{Cx, Subst};
use crate::error::Error;
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

pub(crate) fn record<T, F>(cx: &mut Cx, rows: &[(hir::Lab, T)], mut f: F) -> Ty
where
  T: Copy,
  F: FnMut(&mut Cx, &hir::Lab, T) -> Ty,
{
  let mut ty_rows = BTreeMap::<hir::Lab, Ty>::new();
  for (lab, val) in rows {
    let ty = f(cx, lab, *val);
    match ty_rows.insert(lab.clone(), ty) {
      None => {}
      Some(_) => cx.err(Error::DuplicateLab(lab.clone())),
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

pub(crate) fn instantiate(cx: &mut Cx, ty_scheme: &TyScheme) -> Ty {
  let meta_vars: Vec<_> = cx
    .gen_from_ty_vars(&ty_scheme.vars)
    .map(Ty::MetaVar)
    .collect();
  let mut ty = ty_scheme.ty.clone();
  apply_bv(&meta_vars, &mut ty);
  ty
}

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
