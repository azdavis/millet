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
  ty_entry: Option<TyEntry>,
  def: Option<Def>,
}

impl Info {
  pub(crate) fn insert<I>(&mut self, idx: I, ty_entry: Option<TyEntry>, def: Option<Def>)
  where
    I: Into<hir::Idx>,
  {
    let idx = idx.into();
    // ignore ty schemes that bind no vars
    let entry = InfoEntry {
      ty_entry: ty_entry.map(|mut ty_entry| {
        ty_entry.ty_scheme = ty_entry
          .ty_scheme
          .and_then(|x| (!x.bound_vars.is_empty()).then(|| x));
        ty_entry
      }),
      def,
    };
    assert!(self.store.insert(idx, entry).is_none());
  }

  pub(crate) fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self
      .store
      .values_mut()
      .filter_map(|entry| entry.ty_entry.as_mut().map(|x| &mut x.ty))
  }

  /// Returns a Markdown string with information associated with this index.
  pub fn get_md(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let ty_entry = self.store.get(&idx)?.ty_entry.as_ref()?;
    let mvs = ty_entry.ty.meta_var_names();
    let ty = ty_entry.ty.display(&mvs, syms);
    match &ty_entry.ty_scheme {
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
  pub fn get_def(&self, idx: hir::Idx) -> Option<Def> {
    self.store.get(&idx)?.def
  }
}
