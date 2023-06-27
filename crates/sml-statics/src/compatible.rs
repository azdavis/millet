//! Checking whether type schemes are compatible with each other.

use crate::{error::ErrorKind, st::St, unify::unify_no_emit};
use sml_statics_types::fmt_util::ty_var_name;
use sml_statics_types::ty::{BoundTyVars, Generalizable, Ty, TyScheme, TyVarKind, TyVarSrc};
use sml_statics_types::util::{apply_bv, instantiate};

type Result = std::result::Result<(), ErrorKind>;

/// returns `Ok(())` iff the ty fns are equal. (this is distinct from equal ty schemes because the
/// order of bound variables is significant.)
pub(crate) fn eq_ty_fn_no_emit(st: &mut St<'_>, mut lhs: TyScheme, mut rhs: TyScheme) -> Result {
  if lhs.bound_vars.len() != rhs.bound_vars.len() {
    return Err(ErrorKind::WrongNumTyArgs(lhs.bound_vars.len(), rhs.bound_vars.len()));
  }
  let subst = fixed_var_subst(st, &lhs.bound_vars);
  apply_bv(&mut st.syms_tys.tys, &subst, &mut lhs.ty);
  apply_bv(&mut st.syms_tys.tys, &subst, &mut rhs.ty);
  unify_no_emit(st, lhs.ty, rhs.ty)
}

/// emits no error iff the ty fns are equal.
pub(crate) fn eq_ty_fn(st: &mut St<'_>, idx: sml_hir::Idx, lhs: TyScheme, rhs: TyScheme) {
  match eq_ty_fn_no_emit(st, lhs, rhs) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

/// returns `Ok(())` iff the ty schemes are equal.
pub(crate) fn eq_ty_scheme(st: &mut St<'_>, lhs: &TyScheme, rhs: &TyScheme) -> Result {
  generalizes_no_emit(st, lhs, rhs)?;
  generalizes_no_emit(st, rhs, lhs)?;
  Ok(())
}

fn fixed_var_subst(st: &mut St<'_>, bound_vars: &BoundTyVars) -> Vec<Ty> {
  bound_vars
    .iter()
    .enumerate()
    .map(|(idx, data)| {
      let equality = matches!(data.ty_var_kind(), TyVarKind::Equality);
      let ty_var = ty_var_name(equality, idx).to_string();
      st.syms_tys.tys.fixed_var(sml_hir::TyVar::new(ty_var), TyVarSrc::Ty)
    })
    .collect()
}

fn generalizes_no_emit(st: &mut St<'_>, general: &TyScheme, specific: &TyScheme) -> Result {
  let general = instantiate(&mut st.syms_tys.tys, Generalizable::Always, general);
  let subst = fixed_var_subst(st, &specific.bound_vars);
  let mut specific = specific.ty;
  apply_bv(&mut st.syms_tys.tys, &subst, &mut specific);
  unify_no_emit(st, specific, general)
}

/// emits no error iff `general` generalizes `specific`.
pub(crate) fn generalizes(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  general: &TyScheme,
  specific: &TyScheme,
) {
  match generalizes_no_emit(st, general, specific) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}
