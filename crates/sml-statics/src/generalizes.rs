use crate::fmt_util::ty_var_name;
use crate::st::St;
use crate::types::{Generalizable, Ty, TyScheme, TyVarKind};
use crate::unify::{unify, unify_, Result};
use crate::util::{apply_bv, instantiate};

/// emits no error iff `lhs` and `rhs` are equal ty schemes.
pub(crate) fn eq_ty_scheme(st: &mut St, lhs: TyScheme, rhs: TyScheme, idx: sml_hir::Idx) {
  generalizes(st, lhs.clone(), &rhs, idx);
  generalizes(st, rhs, &lhs, idx);
}

/// we _maybe_ could use `#[derive(PartialEq, Eq)]` for [`Ty`] and [`crate::types::BoundTyVars`],
/// and then just call those here instead of using [`unify_`] (and then return a bool instead of a
/// [`Result`]), since equivalent [`TyScheme`]s _should_ be alpha-equivalent (and thus `==`).
///
/// but... something about that makes me uneasy. maybe it's because we have [`Ty::MetaVar`] and
/// [`Ty::FixedVar`] to deal with? but those should probably not come up in ty schemes too often,
/// right? _shrug_ for now I'm going to just read off the Definition, which says they're eq if they
/// generalize each other.
pub(crate) fn eq_ty_scheme_no_emit(st: &mut St, lhs: TyScheme, rhs: TyScheme) -> Result {
  generalizes_no_emit(st, lhs.clone(), &rhs)?;
  generalizes_no_emit(st, rhs, &lhs)?;
  Ok(())
}

fn prepare_generalize(st: &mut St, general: TyScheme, specific: &TyScheme) -> (Ty, Ty) {
  let general = instantiate(st, general, Generalizable::Always);
  let specific = {
    let subst: Vec<_> = specific
      .bound_vars
      .kinds()
      .enumerate()
      .map(|(idx, kind)| {
        let equality = matches!(kind, Some(TyVarKind::Equality));
        let ty_var = ty_var_name(equality, idx).to_string();
        Ty::FixedVar(st.gen_fixed_var(sml_hir::TyVar::new(ty_var)))
      })
      .collect();
    let mut ty = specific.ty.clone();
    apply_bv(&subst, &mut ty);
    ty
  };
  (general, specific)
}

/// emits no error iff `general` generalizes `specific`.
pub(crate) fn generalizes(st: &mut St, general: TyScheme, specific: &TyScheme, idx: sml_hir::Idx) {
  let (general, specific) = prepare_generalize(st, general, specific);
  unify(st, specific, general, idx)
}

fn generalizes_no_emit(st: &mut St, general: TyScheme, specific: &TyScheme) -> Result {
  let (general, specific) = prepare_generalize(st, general, specific);
  unify_(st, specific, general)
}
