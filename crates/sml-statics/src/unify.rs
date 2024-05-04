//! A thin wrapper around the inner `unify`.

use crate::error::ErrorKind;
use crate::st::St;
use sml_statics_types::ty::Ty;
use sml_statics_types::unify;

pub(crate) fn unify(st: &mut St<'_>, idx: sml_hir::Idx, want: Ty, got: Ty) {
  match unify_no_emit(st.syms_tys, st.info.mode, want, got) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

pub(crate) fn unify_no_emit(
  st: &mut sml_statics_types::St,
  mode: sml_statics_types::mode::Mode,
  want: Ty,
  got: Ty,
) -> Result<(), ErrorKind> {
  if mode.is_path_order() {
    return Ok(());
  }
  unify::unify(&mut st.tys, &st.syms, want, got).map_err(|err| match err {
    unify::Error::Circularity(circ) => ErrorKind::Circularity(circ),
    unify::Error::Incompatible(reason) => ErrorKind::IncompatibleTys(reason, want, got),
  })
}
