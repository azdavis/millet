//! Enrichment of various structures.
//!
//! TODO improve error messages/ranges. for ranges, we might need to track `sml_hir::Idx`es either
//! in the Env or in a separate map with exactly the same keys (names). or we could add a special
//! env only for use here that has the indices?

use crate::compatible::{eq_ty_fn, generalizes};
use crate::{error::ErrorKind, st::St};
use sml_statics_types::info::{IdStatus, TyInfo, ValInfo};
use sml_statics_types::{env::Env, item::Item};

pub(crate) fn get_env(st: &mut St, idx: sml_hir::Idx, general: &Env, specific: &Env) {
  for (name, specific) in specific.str_env.iter() {
    match general.str_env.get(name) {
      Some(general) => get_env(st, idx, general, specific),
      None => st.err(idx, ErrorKind::Missing(Item::Struct, name.clone())),
    }
  }
  for (name, specific) in specific.ty_env.iter() {
    match general.ty_env.get(name) {
      Some(general) => get_ty_info(st, idx, general.clone(), specific.clone()),
      None => st.err(idx, ErrorKind::Missing(Item::Ty, name.clone())),
    }
  }
  for (name, specific) in specific.val_env.iter() {
    match general.val_env.get(name) {
      Some(general) => get_val_info(st, idx, general, specific, name),
      None => st.err(idx, ErrorKind::Missing(Item::Val, name.clone())),
    }
  }
}

fn get_ty_info(st: &mut St, idx: sml_hir::Idx, mut general: TyInfo, specific: TyInfo) {
  eq_ty_fn(st, idx, specific.ty_scheme, general.ty_scheme.clone());
  if specific.val_env.is_empty() {
    return;
  }
  for (name, specific) in specific.val_env {
    match general.val_env.remove(&name) {
      Some(general) => {
        if !general.id_status.same_kind_as(specific.id_status) {
          st.err(idx, ErrorKind::WrongIdStatus(name.clone()));
        }
        eq_ty_fn(st, idx, specific.ty_scheme, general.ty_scheme.clone());
      }
      None => st.err(idx, ErrorKind::Missing(Item::Val, name.clone())),
    }
  }
  for (name, _) in general.val_env.iter() {
    st.err(idx, ErrorKind::Extra(Item::Val, name.clone()));
  }
}

fn get_val_info(
  st: &mut St,
  idx: sml_hir::Idx,
  general: &ValInfo,
  specific: &ValInfo,
  name: &str_util::Name,
) {
  generalizes(st, idx, &general.ty_scheme, &specific.ty_scheme);
  if !general.id_status.same_kind_as(specific.id_status)
    && !matches!(specific.id_status, IdStatus::Val)
  {
    st.err(idx, ErrorKind::WrongIdStatus(name.clone()));
  }
}
