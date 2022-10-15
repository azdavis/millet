use crate::error::ErrorKind;
use crate::fmt_util::ty_var_name;
use crate::st::St;
use crate::types::{BoundTyVars, Generalizable, Ty, TyScheme, TyVarKind, TyVarSrc};
use crate::unify::unify_no_emit;
use crate::util::{apply_bv, instantiate};

type Result = std::result::Result<(), ErrorKind>;

pub(crate) fn eq_ty_scheme_no_emit(st: &mut St, lhs: &TyScheme, rhs: TyScheme) -> Result {
  generalizes_no_emit(st, lhs.clone(), &rhs)?;
  generalizes_no_emit(st, rhs, lhs)?;
  Ok(())
}

/// emits no error iff `lhs` and `rhs` are equal ty schemes.
pub(crate) fn eq_ty_scheme(st: &mut St, lhs: &TyScheme, rhs: TyScheme, idx: sml_hir::Idx) {
  match eq_ty_scheme_no_emit(st, lhs, rhs) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

fn fixed_var_subst(st: &mut St, bound_vars: &BoundTyVars) -> Vec<Ty> {
  bound_vars
    .kinds()
    .enumerate()
    .map(|(idx, kind)| {
      let equality = matches!(kind, Some(TyVarKind::Equality));
      let ty_var = ty_var_name(equality, idx).to_string();
      Ty::FixedVar(st.gen_fixed_var(sml_hir::TyVar::new(ty_var), TyVarSrc::Ty))
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
