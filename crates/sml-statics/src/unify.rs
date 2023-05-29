//! A thin wrapper around the inner `unify`.

use crate::error::ErrorKind;
use crate::st::St;
use sml_statics_types::ty::Ty;
use sml_statics_types::unify;

pub(crate) fn unify(st: &mut St<'_>, idx: sml_hir::Idx, want: Ty, got: Ty) {
  match unify_no_emit(st, want, got) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

pub(crate) fn unify_no_emit(st: &mut St<'_>, want: Ty, got: Ty) -> Result<(), ErrorKind> {
  if st.info.mode.is_path_order() {
    return Ok(());
  }
  unify::unify(&mut st.syms_tys.tys, &st.syms_tys.syms, want, got).map_err(|err| match err {
    unify::Error::Circularity(circ) => ErrorKind::Circularity(circ),
    unify::Error::Incompatible(reason) => ErrorKind::IncompatibleTys(reason, want, got),
  })
}
