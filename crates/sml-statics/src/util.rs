//! Misc utilities for inserting into maps and constructing records.

use crate::{error::ErrorKind, st::St};
use chain_map::ChainMap;
use sml_statics_types::item::Item;
use sml_statics_types::ty::{RecordData, Ty};

/// @def(6), @def(39), @def(49)
pub(crate) fn record<T, F>(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  rows: &[(sml_hir::Lab, T)],
  mut f: F,
) -> RecordData
where
  T: Copy,
  F: FnMut(&mut St<'_>, &sml_hir::Lab, T) -> Ty,
{
  let mut ty_rows = RecordData::new();
  for (lab, val) in rows {
    let ty = f(st, lab, *val);
    match ty_rows.insert(lab.clone(), ty) {
      None => {}
      Some(_) => st.err(idx, ErrorKind::DuplicateLab(lab.clone())),
    }
  }
  ty_rows
}

/// inserts `(name, val)` into the map, but returns `Some(e)` if `name` was already a key, where `e`
/// is an error describing this transgression.
pub(crate) fn ins_no_dupe<V>(
  map: &mut ChainMap<str_util::Name, V>,
  name: str_util::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  (!map.insert(name.clone(), val)).then_some(ErrorKind::Duplicate(item, name))
}

/// inerts a name that is not one of the special reserved names like `true`.
pub(crate) fn ins_check_name<V>(
  map: &mut ChainMap<str_util::Name, V>,
  name: str_util::Name,
  val: V,
  item: Item,
) -> Option<ErrorKind> {
  check_name(&name).or_else(|| ins_no_dupe(map, name, val, item))
}

/// returns an error iff this name is not allowed to be (re)bound
pub(crate) fn check_name(name: &str_util::Name) -> Option<ErrorKind> {
  let no = matches!(name.as_str(), "true" | "false" | "nil" | "::" | "ref" | "=" | "it");
  no.then(|| ErrorKind::InvalidRebindName(name.clone()))
}
