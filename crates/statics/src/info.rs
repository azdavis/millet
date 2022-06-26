use crate::types::{Def, Syms, Ty, TyScheme};
use fast_hash::FxHashMap;

/// Information about HIR indices.
#[derive(Debug, Default)]
pub struct Info {
  store: FxHashMap<hir::Idx, InfoEntry>,
}

#[derive(Debug)]
struct InfoEntry {
  ty: Ty,
  ty_scheme: Option<TyScheme>,
  def: Option<Def>,
}

impl Info {
  pub(crate) fn insert<I>(&mut self, idx: I, ty: Ty, ty_scheme: Option<TyScheme>)
  where
    I: Into<hir::Idx>,
  {
    let idx = idx.into();
    let entry = InfoEntry {
      ty,
      def: None,
      // ignore ty schemes that bind no vars
      ty_scheme: ty_scheme.and_then(|ts| (!ts.bound_vars.is_empty()).then(|| ts)),
    };
    assert!(self.store.insert(idx, entry).is_none());
  }

  pub(crate) fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self.store.values_mut().map(|entry| &mut entry.ty)
  }

  /// Returns a Markdown string with information associated with this index.
  pub fn get_md_info(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let entry = self.store.get(&idx)?;
    let mvs = entry.ty.meta_var_names();
    let ty = entry.ty.display(&mvs, syms);
    match &entry.ty_scheme {
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
