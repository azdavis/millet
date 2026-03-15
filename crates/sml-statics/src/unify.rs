//! A thin wrapper around the inner `unify`.

use crate::error::ErrorKind;
use crate::st::St;
use sml_statics_types::ty::Ty;
use sml_statics_types::unify;

pub(crate) fn unify(st: &mut St<'_>, idx: sml_hir::Idx, want: Ty, got: Ty) {
  match unify_inner(st.syms_tys, st.info.mode, Some(idx), want, got) {
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
  unify_inner(st, mode, None, want, got)
}

fn unify_inner(
  st: &mut sml_statics_types::St,
  mode: sml_statics_types::mode::Mode,
  idx: Option<sml_hir::Idx>,
  want: Ty,
  got: Ty,
) -> Result<(), ErrorKind> {
  if mode.is_path_order() {
    return Ok(());
  }
  if let Some(notes) = &mut st.notes {
    notes.enter(idx);
  }
  let res = unify::unify(&mut st.notes, &mut st.tys, &st.syms, want, got);
  if let Some(notes) = &mut st.notes {
    notes.exit();
  }
  res.map_err(|err| {
    let events =
      st.notes.as_mut().map(|notes| notes.get_events(&st.tys, vec![want, got])).unwrap_or_default();
    ErrorKind::Unify(err, want, got, Box::new(events))
  })
}
