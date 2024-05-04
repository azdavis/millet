//! Checking whether type schemes are compatible with each other.

use crate::{error::ErrorKind, st::St, unify::unify_no_emit};
use sml_statics_types::ty::{BoundTyVars, Generalizable, Ty, TyScheme, TyVarKind, TyVarSrc};
use sml_statics_types::util::{apply_bv, instantiate};

type Result = std::result::Result<(), ErrorKind>;

/// returns `Ok(())` iff the ty fns are equal. (this is distinct from equal ty schemes because the
/// order of bound variables is significant.)
pub(crate) fn eq_ty_fn_no_emit(
  st: &mut sml_statics_types::St,
  mode: sml_statics_types::mode::Mode,
  mut lhs: TyScheme,
  mut rhs: TyScheme,
) -> Result {
  if lhs.bound_vars.len() != rhs.bound_vars.len() {
    return Err(ErrorKind::WrongNumTyArgs(lhs.bound_vars.len(), rhs.bound_vars.len()));
  }
  let subst = fixed_var_subst(st, &lhs.bound_vars);
  apply_bv(&mut st.tys, &subst, &mut lhs.ty);
  apply_bv(&mut st.tys, &subst, &mut rhs.ty);
  unify_no_emit(st, mode, lhs.ty, rhs.ty)
}

/// emits no error iff the ty fns are equal.
pub(crate) fn eq_ty_fn(st: &mut St<'_>, idx: sml_hir::Idx, lhs: TyScheme, rhs: TyScheme) {
  match eq_ty_fn_no_emit(st.syms_tys, st.info.mode, lhs, rhs) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

/// returns `Ok(())` iff the ty schemes are equal.
pub(crate) fn eq_ty_scheme(
  st: &mut sml_statics_types::St,
  mode: sml_statics_types::mode::Mode,
  lhs: &TyScheme,
  rhs: &TyScheme,
) -> Result {
  generalizes_no_emit(st, mode, lhs, rhs)?;
  generalizes_no_emit(st, mode, rhs, lhs)?;
  Ok(())
}

fn fixed_var_subst(st: &mut sml_statics_types::St, bound_vars: &BoundTyVars) -> Vec<Ty> {
  bound_vars
    .iter()
    .enumerate()
    .map(|(idx, data)| {
      let equality = matches!(data.ty_var_kind(), TyVarKind::Equality);
      let ty_var = sml_hir::UnutterableTyVar::new(equality, idx);
      st.tys.fixed_var(sml_hir::TyVar::unutterable(ty_var), TyVarSrc::Ty)
    })
    .collect()
}

/// returns `Ok(())` iff `general` generalizes `specific`.
fn generalizes_no_emit(
  st: &mut sml_statics_types::St,
  mode: sml_statics_types::mode::Mode,
  general: &TyScheme,
  specific: &TyScheme,
) -> Result {
  let general = instantiate(&mut st.tys, Generalizable::Always, general);
  let subst = fixed_var_subst(st, &specific.bound_vars);
  let mut specific = specific.ty;
  apply_bv(&mut st.tys, &subst, &mut specific);
  unify_no_emit(st, mode, specific, general)
}

/// emits no error iff `general` generalizes `specific`.
pub(crate) fn generalizes(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  general: &TyScheme,
  specific: &TyScheme,
) {
  match generalizes_no_emit(st.syms_tys, st.info.mode, general, specific) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}
