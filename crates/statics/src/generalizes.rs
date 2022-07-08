use crate::fmt_util::ty_var_name;
use crate::st::St;
use crate::types::{Ty, TyScheme, TyVarKind};
use crate::unify::unify;
use crate::util::{apply_bv, instantiate};

/// emits no error iff `lhs` and `rhs` are equal ty schemes.
pub(crate) fn eq_ty_scheme(st: &mut St, lhs: TyScheme, rhs: TyScheme, idx: hir::Idx) {
  // TODO just use `==` since alpha equivalent ty schemes are already `==` for derive(PartialEq)?
  generalizes(st, lhs.clone(), &rhs, idx);
  generalizes(st, rhs, &lhs, idx);
}

/// emits no error iff `general` generalizes `specific`.
pub(crate) fn generalizes(st: &mut St, general: TyScheme, specific: &TyScheme, idx: hir::Idx) {
  let general = instantiate(st, general);
  let specific = {
    let subst: Vec<_> = specific
      .bound_vars
      .kinds()
      .enumerate()
      .map(|(idx, kind)| {
        let equality = matches!(kind, Some(TyVarKind::Equality));
        let ty_var = ty_var_name(equality, idx).to_string();
        Ty::FixedVar(st.gen_fixed_var(hir::TyVar::new(ty_var)))
      })
      .collect();
    let mut ty = specific.ty.clone();
    apply_bv(&subst, &mut ty);
    ty
  };
  unify(st, specific, general, idx)
}
