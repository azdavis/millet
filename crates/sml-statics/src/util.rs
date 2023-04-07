//! Utilities.

use crate::sym::Sym;
use crate::ty_var::meta::{Generalizable, MetaTyVar};
use crate::types::{RecordTy, Subst, SubstEntry, Ty, TyScheme, TyVarKind};
use crate::{error::ErrorKind, item::Item, overload, st::St};
use chain_map::ChainMap;

pub(crate) fn get_scon(st: &mut St, g: Generalizable, scon: &sml_hir::SCon) -> Ty {
  // we could have all of these return the basic overloads, but some of them will all only allow the
  // default types anyway.
  match scon {
    sml_hir::SCon::Int(_) => basic_overload(st, g, overload::Basic::Int),
    sml_hir::SCon::Real(_) => basic_overload(st, g, overload::Basic::Real),
    sml_hir::SCon::Word(_) => basic_overload(st, g, overload::Basic::Word),
    sml_hir::SCon::Char(_) => Ty::CHAR,
    sml_hir::SCon::String(_) => Ty::STRING,
  }
}

fn basic_overload(st: &mut St, g: Generalizable, b: overload::Basic) -> Ty {
  let mv = st.meta_gen.gen(g);
  let entry = SubstEntry::Kind(TyVarKind::Overloaded(b.into()));
  st.subst.insert(mv, entry);
  Ty::MetaVar(mv)
}

/// @def(6), @def(39), @def(49)
pub(crate) fn record<T, F>(
  st: &mut St,
  idx: sml_hir::Idx,
  rows: &[(sml_hir::Lab, T)],
  mut f: F,
) -> RecordTy
where
  T: Copy,
  F: FnMut(&mut St, &sml_hir::Lab, T) -> Ty,
{
  let mut ty_rows = RecordTy::new();
  for (lab, val) in rows {
    let ty = f(st, lab, *val);
    match ty_rows.insert(lab.clone(), ty) {
      None => {}
      Some(_) => st.err(idx, ErrorKind::DuplicateLab(lab.clone())),
    }
  }
  ty_rows
}

/// substitute any meta type variables in `ty` with their types in `subst`.
///
/// meta variables not defined by the `subst` are left alone.
pub(crate) fn apply(subst: &Subst, ty: &mut Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None | Some(SubstEntry::Kind(_)) => {}
      Some(SubstEntry::Solved(t)) => {
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

/// instantiates the type scheme's type with new meta type vars, according to the bound vars of the
/// type scheme.
pub(crate) fn instantiate(st: &mut St, g: Generalizable, ty_scheme: TyScheme) -> Ty {
  let subst: Vec<_> = ty_scheme
    .bound_vars
    .iter()
    .map(|x| {
      let mv = st.meta_gen.gen(g);
      if let Some(k) = x {
        let k = SubstEntry::Kind(k.clone());
        assert!(st.subst.insert(mv, k).is_none());
      }
      Ty::MetaVar(mv)
    })
    .collect();
  let mut ty = ty_scheme.ty;
  apply_bv(&subst, &mut ty);
  apply(&st.subst, &mut ty);
  ty
}

/// like [`apply`], but for bound type variables.
///
/// unlike `apply`, all bound variables must be defined by the subst.
pub(crate) fn apply_bv(subst: &[Ty], ty: &mut Ty) {
  match ty {
    Ty::None | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
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

/// inserts (name, val) into the map, but returns Some(e) if name was already a key, where e is an
/// error describing this transgression.
pub(crate) fn ins_no_dupe<V>(
  map: &mut ChainMap<str_util::Name, V>,
  name: str_util::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  (!map.insert(name.clone(), val)).then_some(ErrorKind::Duplicate(item, name))
}

pub(crate) fn ins_check_name<V>(
  map: &mut ChainMap<str_util::Name, V>,
  name: str_util::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  let no = matches!(name.as_str(), "true" | "false" | "nil" | "::" | "ref" | "=" | "it");
  no.then(|| ErrorKind::InvalidRebindName(name.clone()))
    .or_else(|| ins_no_dupe(map, name, val, item))
}

pub(crate) fn ty_syms<F: FnMut(Sym)>(ty: &Ty, f: &mut F) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values() {
        ty_syms(ty, f);
      }
    }
    Ty::Con(args, sym) => {
      for ty in args {
        ty_syms(ty, f);
      }
      f(*sym);
    }
    Ty::Fn(param, res) => {
      ty_syms(param, f);
      ty_syms(res, f);
    }
  }
}

pub(crate) fn meta_vars<F>(subst: &Subst, ty: &Ty, f: &mut F)
where
  F: FnMut(MetaTyVar, Option<&TyVarKind>),
{
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None => f(*mv, None),
      Some(SubstEntry::Kind(k)) => f(*mv, Some(k)),
      Some(SubstEntry::Solved(ty)) => meta_vars(subst, ty, f),
    },
    Ty::Record(rows) => {
      for ty in rows.values() {
        meta_vars(subst, ty, f);
      }
    }
    Ty::Con(args, _) => {
      for ty in args.iter() {
        meta_vars(subst, ty, f);
      }
    }
    Ty::Fn(param, res) => {
      meta_vars(subst, param, f);
      meta_vars(subst, res, f);
    }
  }
}
