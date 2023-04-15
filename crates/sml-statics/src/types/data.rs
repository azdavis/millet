//! See [`Map`].

use fast_hash::FxHashMap;

/// A two-way mapping between interned indices and data.
#[derive(Debug, Clone)]
pub(crate) struct Map<T> {
  idx: FxHashMap<T, idx::Idx>,
  data: Vec<T>,
}

impl<T> Map<T> {
  pub(crate) fn get_idx(&mut self, data: T) -> idx::Idx
  where
    T: Clone + Eq + std::hash::Hash,
  {
    if let Some(&ret) = self.idx.get(&data) {
      return ret;
    }
    let ret = idx::Idx::new(self.data.len());
    assert!(self.idx.insert(data.clone(), ret).is_none());
    self.data.push(data);
    ret
  }

  pub(crate) fn get_data(&self, idx: idx::Idx) -> &T {
    &self.data[idx.to_usize()]
  }
}

impl<T> Default for Map<T> {
  fn default() -> Self {
    Self { idx: FxHashMap::default(), data: Vec::new() }
  }
}
