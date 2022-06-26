use crate::types::{Syms, Ty, TyScheme};
use fast_hash::FxHashMap;

/// Information about HIR indices.
#[derive(Debug, Default)]
pub struct Info {
  store: FxHashMap<hir::Idx, (Ty, Option<TyScheme>)>,
}

impl Info {
  pub(crate) fn insert<I>(&mut self, idx: I, ty: Ty, ty_scheme: Option<TyScheme>)
  where
    I: Into<hir::Idx>,
  {
    let idx = idx.into();
    // ignore ty schemes that bind no vars
    let ty_scheme = ty_scheme.and_then(|x| (!x.bound_vars.is_empty()).then(|| x));
    assert!(self.store.insert(idx, (ty, ty_scheme)).is_none());
  }

  pub(crate) fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self.store.values_mut().map(|(ty, _)| ty)
  }

  /// Returns a Markdown string with information associated with this index.
  pub fn get_md_info(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let (ty, ty_scheme) = self.store.get(&idx)?;
    let mvs = ty.meta_var_names();
    let ty = ty.display(&mvs, syms);
    match ty_scheme {
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
}
