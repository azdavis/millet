use crate::types::{Def, Syms, Ty, TyScheme};
use fast_hash::FxHashMap;

/// Information about HIR indices.
#[derive(Debug, Default)]
pub struct Info {
  store: FxHashMap<hir::Idx, InfoEntry>,
}

#[derive(Debug)]
pub(crate) struct TyEntry {
  pub(crate) ty: Ty,
  pub(crate) ty_scheme: Option<TyScheme>,
}

#[derive(Debug)]
struct InfoEntry {
  ty_entry: TyEntry,
  def: Option<Def>,
}

impl Info {
  pub(crate) fn insert<I>(&mut self, idx: I, mut ty_entry: TyEntry, def: Option<Def>)
  where
    I: Into<hir::Idx>,
  {
    let idx = idx.into();
    // ignore ty schemes that bind no vars
    ty_entry.ty_scheme = ty_entry
      .ty_scheme
      .and_then(|x| (!x.bound_vars.is_empty()).then(|| x));
    let entry = InfoEntry { ty_entry, def };
    assert!(self.store.insert(idx, entry).is_none());
  }

  pub(crate) fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self.store.values_mut().map(|entry| &mut entry.ty_entry.ty)
  }

  /// Returns a Markdown string with information associated with this index.
  pub fn get_md_info(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let entry = self.store.get(&idx)?;
    let mvs = entry.ty_entry.ty.meta_var_names();
    let ty = entry.ty_entry.ty.display(&mvs, syms);
    match &entry.ty_entry.ty_scheme {
      None => Some(format!("```sml\n{ty}\n```")),
      Some(ty_scheme) => {
        let mvs = ty_scheme.ty.meta_var_names();
        let ty_scheme = ty_scheme.display(&mvs, syms);
        Some(format!(
          "```sml\n(* most general *)\n{ty_scheme}\n(* this usage *)\n{ty}\n```"
        ))
      }
    }
  }

  /// Returns the definition site of the idx.
  pub fn get_def_location(&self, idx: hir::Idx) -> Option<Def> {
    self.store.get(&idx)?.def
  }
}
