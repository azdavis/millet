use crate::error::{ErrorKind, Item};
use crate::st::St;
use crate::types::{Env, Subst, SubstEntry, Sym, Ty, TyScheme};
use fast_hash::FxHashMap;
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

/// sml_def(6), sml_def(39), sml_def(49)
pub(crate) fn record<T, F, I>(st: &mut St, rows: &[(hir::Lab, T)], idx: I, mut f: F) -> Ty
where
  T: Copy,
  F: FnMut(&mut St, &hir::Lab, T) -> Ty,
  I: Into<hir::Idx>,
{
  let mut ty_rows = BTreeMap::<hir::Lab, Ty>::new();
  let idx = idx.into();
  for (lab, val) in rows {
    let ty = f(st, lab, *val);
    match ty_rows.insert(lab.clone(), ty) {
      None => {}
      Some(_) => st.err(idx, ErrorKind::DuplicateLab(lab.clone())),
    }
  }
  Ty::Record(ty_rows)
}

pub(crate) fn get_env<'e, 'n, I>(mut env: &'e Env, names: I) -> Result<&'e Env, &'n hir::Name>
where
  I: IntoIterator<Item = &'n hir::Name>,
{
  for name in names {
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
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(mv) {
      None | Some(SubstEntry::Kind(_)) => {}
      Some(SubstEntry::Set(t)) => {
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
pub(crate) fn instantiate(st: &mut St, ty_scheme: &TyScheme) -> Ty {
  let subst: Vec<_> = st.gen_from(&ty_scheme.bound_vars).collect();
  let mut ty = ty_scheme.ty.clone();
  apply_bv(&subst, &mut ty);
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

pub(crate) fn cannot_bind(s: &str) -> bool {
  matches!(s, "true" | "false" | "nil" | "::" | "ref")
}

#[must_use]
pub(crate) fn ins_no_dupe<V>(
  map: &mut FxHashMap<hir::Name, V>,
  name: hir::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  map
    .insert(name.clone(), val)
    .map(|_| ErrorKind::Duplicate(item, name))
}
