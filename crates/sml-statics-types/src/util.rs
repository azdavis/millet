//! Utilities.

use crate::ty::{BoundTyVar, BoundTyVars, Generalizable, Ty, TyData, TyScheme, TyVarKind, Tys};
use crate::{St, overload, sym::Sym};

/// Gets the type from a special constructor aka literal.
pub fn get_scon(st: &mut St, g: Generalizable, scon: &sml_hir::SCon, idx: sml_hir::Idx) -> Ty {
  // we could have all of these return the basic overloads, but there are no overloads for `char` or
  // `string`, so just return the primitive types themselves for those.
  let basic = match scon {
    sml_hir::SCon::Char(_) => return Ty::CHAR,
    sml_hir::SCon::String(_) => return Ty::STRING,
    sml_hir::SCon::Int(_) => overload::Basic::Int,
    sml_hir::SCon::Real(_) => overload::Basic::Real,
    sml_hir::SCon::Word(_) => overload::Basic::Word,
  };
  let ov = overload::Overload::from(basic);
  let kind = TyVarKind::Overloaded(ov);
  let ret = st.tys.meta_var(g, kind);
  if let Some(notes) = &mut st.notes {
    notes.introduce_overloaded(ret, idx, ov);
  }
  ret
}

/// Instantiates the type scheme's type with new meta type vars, according to the bound vars of the
/// type scheme.
pub fn instantiate(tys: &mut Tys, g: Generalizable, ty_scheme: &TyScheme) -> Ty {
  let subst: Vec<_> =
    ty_scheme.bound_vars.iter().map(|data| tys.meta_var(g, data.ty_var_kind())).collect();
  let mut ret = ty_scheme.ty;
  apply_bv(tys, &subst, &mut ret);
  ret
}

/// Apply the subst for bound type variables. All bound variables must be defined by the subst.
pub fn apply_bv(tys: &mut Tys, subst: &[Ty], ty: &mut Ty) {
  match tys.data(*ty) {
    // interesting case
    TyData::BoundVar(bv) => *ty = *bv.index_into(subst),
    // trivial base cases
    TyData::None | TyData::UnsolvedMetaVar(_) | TyData::FixedVar(_) => {}
    // recursive cases
    TyData::Record(mut rows) => {
      for ty in rows.values_mut() {
        apply_bv(tys, subst, ty);
      }
      *ty = tys.record(rows);
    }
    TyData::Con(mut data) => {
      for ty in &mut data.args {
        apply_bv(tys, subst, ty);
      }
      *ty = tys.con(data.args, data.sym);
    }
    TyData::Fn(mut data) => {
      apply_bv(tys, subst, &mut data.param);
      apply_bv(tys, subst, &mut data.res);
      *ty = tys.fun(data.param, data.res);
    }
  }
}

/// Calls `f` for each `Sym` in `ty`.
pub fn ty_syms<F: FnMut(Sym)>(tys: &Tys, ty: Ty, f: &mut F) {
  match tys.data(ty) {
    // interesting case
    TyData::Con(data) => {
      for &ty in &data.args {
        ty_syms(tys, ty, f);
      }
      f(data.sym);
    }
    // trivial base cases
    TyData::None | TyData::BoundVar(_) | TyData::UnsolvedMetaVar(_) | TyData::FixedVar(_) => {}
    // recursive cases
    TyData::Record(rows) => {
      for &ty in rows.values() {
        ty_syms(tys, ty, f);
      }
    }
    TyData::Fn(data) => {
      ty_syms(tys, data.param, f);
      ty_syms(tys, data.res, f);
    }
  }
}

/// Returns a ty scheme with the given bound vars, whose type is the constructor type given by the
/// symbol applied to the bound vars as the arguments.
pub fn n_ary_con(tys: &mut Tys, bound_vars: BoundTyVars, sym: Sym) -> TyScheme {
  let args: Vec<_> =
    BoundTyVar::iter_for(bound_vars.iter()).map(|(bv, _)| Ty::bound_var(bv)).collect();
  let ty = tys.con(args, sym);
  TyScheme { bound_vars, ty }
}
