//! Given a `ValInfo`, return the kind of symbol this val is.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use sml_statics_types::info::{IdStatus, ValInfo};
use sml_statics_types::ty::{TyData, Tys};

/// Gets the symbol kind.
#[must_use]
pub fn get(tys: &Tys, val_info: &ValInfo) -> sml_namespace::SymbolKind {
  match val_info.id_status {
    IdStatus::Con => sml_namespace::SymbolKind::Constructor,
    IdStatus::Exn(_) => sml_namespace::SymbolKind::Exception,
    IdStatus::Val => match tys.data(val_info.ty_scheme.ty) {
      TyData::Fn(_) => sml_namespace::SymbolKind::Function,
      _ => sml_namespace::SymbolKind::Value,
    },
  }
}
