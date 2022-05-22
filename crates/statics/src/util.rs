use crate::cx::Cx;
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
