//! Checking whether type schemes are compatible with each other.

use crate::error::ErrorKind;
use crate::fmt_util::ty_var_name;
use crate::st::St;
use crate::types::{BoundTyVars, Generalizable, Ty, TyScheme, TyVarKind, TyVarSrc};
use crate::unify::unify_no_emit;
use crate::util::{apply_bv, instantiate};

type Result = std::result::Result<(), ErrorKind>;

/// returns `Ok(())` iff the ty fns are equal. (this is distinct from equal ty schemes because the
/// order of bound variables is significant.)
pub(crate) fn eq_ty_fn_no_emit(st: &mut St, mut lhs: TyScheme, mut rhs: TyScheme) -> Result {
  if lhs.bound_vars.len() != rhs.bound_vars.len() {
    return Err(ErrorKind::WrongNumTyArgs(lhs.bound_vars.len(), rhs.bound_vars.len()));
  }
  let subst = fixed_var_subst(st, &lhs.bound_vars);
  apply_bv(&subst, &mut lhs.ty);
  apply_bv(&subst, &mut rhs.ty);
  unify_no_emit(st, lhs.ty, rhs.ty)
}

/// emits no error iff the ty fns are equal.
pub(crate) fn eq_ty_fn(st: &mut St, lhs: TyScheme, rhs: TyScheme, idx: sml_hir::Idx) {
  match eq_ty_fn_no_emit(st, lhs, rhs) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

/// returns `Ok(())` iff the ty schemes are equal.
pub(crate) fn eq_ty_scheme(st: &mut St, lhs: &TyScheme, rhs: TyScheme) -> Result {
  generalizes_no_emit(st, lhs.clone(), &rhs)?;
  generalizes_no_emit(st, rhs, lhs)?;
  Ok(())
}

fn fixed_var_subst(st: &mut St, bound_vars: &BoundTyVars) -> Vec<Ty> {
  bound_vars
    .iter()
    .enumerate()
    .map(|(idx, kind)| {
      let equality = matches!(kind, Some(TyVarKind::Equality));
      let ty_var = ty_var_name(equality, idx).to_string();
      Ty::FixedVar(st.fixed_gen.gen(sml_hir::TyVar::new(ty_var), TyVarSrc::Ty))
    })
    .collect()
}

fn generalizes_no_emit(st: &mut St, general: TyScheme, specific: &TyScheme) -> Result {
  let general = instantiate(st, Generalizable::Always, general);
  let subst = fixed_var_subst(st, &specific.bound_vars);
  let mut specific = specific.ty.clone();
  apply_bv(&subst, &mut specific);
  unify_no_emit(st, specific, general)
}

/// emits no error iff `general` generalizes `specific`.
pub(crate) fn generalizes(st: &mut St, general: TyScheme, specific: &TyScheme, idx: sml_hir::Idx) {
  match generalizes_no_emit(st, general, specific) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}
