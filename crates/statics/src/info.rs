use crate::types::{Syms, Ty};
use fast_hash::FxHashMap;

/// Information about HIR indices.
#[derive(Debug, Default)]
pub struct Info {
  store: FxHashMap<hir::Idx, Ty>,
}

impl Info {
  pub(crate) fn insert<I>(&mut self, idx: I, ty: Ty)
  where
    I: Into<hir::Idx>,
  {
    let idx = idx.into();
    assert!(self.store.insert(idx, ty).is_none());
  }

  pub(crate) fn values_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
    self.store.values_mut()
  }

  /// Returns a Markdown string with information associated with this index.
  pub fn get(&self, syms: &Syms, idx: hir::Idx) -> Option<String> {
    let ty = self.store.get(&idx)?;
    let d = ty.display(syms);
    Some(format!("```sml\n{d}\n```"))
  }
}
