use crate::cx::{Cx, Subst};
use crate::error::Error;
use crate::types::{Sym, Ty};
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
  F: FnMut(&mut Cx, T) -> Ty,
{
  let mut ty_rows = BTreeMap::<hir::Lab, Ty>::new();
  for (lab, val) in rows {
    let ty = f(cx, *val);
    match ty_rows.insert(lab.clone(), ty) {
      None => {}
      Some(_) => cx.err(Error::DuplicateLab(lab.clone())),
    }
  }
  Ty::Record(ty_rows)
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
