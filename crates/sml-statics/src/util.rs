use crate::error::{ErrorKind, Item};
use crate::st::St;
use crate::types::{
  BasicOverload, Generalizable, Overload, RecordTy, Subst, SubstEntry, Sym, Ty, TyScheme, TyVarKind,
};
use fast_hash::FxHashMap;

pub(crate) fn get_scon(st: &mut St, g: Generalizable, scon: &sml_hir::SCon) -> Ty {
  // we could have all of these return the basic overloads, but some of them will all only allow the
  // default types anyway.
  match scon {
    sml_hir::SCon::Int(_) => basic_overload(st, g, BasicOverload::Int),
    sml_hir::SCon::Real(_) => basic_overload(st, g, BasicOverload::Real),
    sml_hir::SCon::Word(_) => basic_overload(st, g, BasicOverload::Word),
    sml_hir::SCon::Char(_) => Ty::CHAR,
    sml_hir::SCon::String(_) => Ty::STRING,
  }
}

fn basic_overload(st: &mut St, g: Generalizable, b: BasicOverload) -> Ty {
  let mv = st.meta_gen.gen(g);
  let entry = SubstEntry::Kind(TyVarKind::Overloaded(Overload::Basic(b)));
  st.subst().insert(mv, entry);
  Ty::MetaVar(mv)
}

/// sml_def(6), sml_def(39), sml_def(49)
pub(crate) fn record<T, F>(
  st: &mut St,
  rows: &[(sml_hir::Lab, T)],
  idx: sml_hir::Idx,
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
    Ty::MetaVar(mv) => match subst.get(mv) {
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
    .kinds()
    .map(|x| {
      let mv = st.meta_gen.gen(g);
      if let Some(k) = x {
        let k = SubstEntry::Kind(k.clone());
        assert!(st.subst().insert(mv, k).is_none())
      }
      Ty::MetaVar(mv)
    })
    .collect();
  let mut ty = ty_scheme.ty;
  apply_bv(&subst, &mut ty);
  apply(st.subst(), &mut ty);
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
  map: &mut FxHashMap<str_util::Name, V>,
  name: str_util::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  map.insert(name.clone(), val).map(|_| ErrorKind::Duplicate(item, name))
}

pub(crate) fn ins_check_name<V>(
  map: &mut FxHashMap<str_util::Name, V>,
  name: str_util::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  let no = matches!(name.as_str(), "true" | "false" | "nil" | "::" | "ref" | "=" | "it");
  no.then(|| ErrorKind::InvalidRebindName(name.clone()))
    .or_else(|| ins_no_dupe(map, name, val, item))
}

pub(crate) fn ty_syms<F: FnMut(Sym)>(f: &mut F, ty: &Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values() {
        ty_syms(f, ty);
      }
    }
    Ty::Con(args, sym) => {
      for ty in args {
        ty_syms(f, ty);
      }
      f(*sym);
    }
    Ty::Fn(param, res) => {
      ty_syms(f, param);
      ty_syms(f, res);
    }
  }
}

/// useful for closures that add/remove things from sets, since those methods return `bool`.
pub(crate) fn ignore(_: bool) {}
