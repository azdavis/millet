//! A thin wrapper around the inner `unify`.

use crate::error::ErrorKind;
use crate::st::St;
use crate::types::ty::Ty;
use crate::types::unify::{self, UnifyError};

pub(crate) fn unify(st: &mut St, idx: sml_hir::Idx, want: Ty, got: Ty) {
  match unify_no_emit(st, want, got) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

pub(crate) fn unify_no_emit(st: &mut St, want: Ty, got: Ty) -> Result<(), ErrorKind> {
  if st.info.mode.is_path_order() {
    return Ok(());
  }
  unify::unify(&mut st.tys, &st.syms, want, got).map_err(|err| match err {
    UnifyError::Circularity(circ) => ErrorKind::Circularity(circ),
    UnifyError::Incompatible(reason) => ErrorKind::IncompatibleTys(reason, want, got),
  })
}
