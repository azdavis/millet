//! A thin wrapper over [`rustc_hash`] with some extra helper functions.

use std::hash::{BuildHasherDefault, Hash};

pub use rustc_hash::{FxHashMap, FxHashSet};

pub fn map_with_capacity<K, V>(cap: usize) -> FxHashMap<K, V> {
  FxHashMap::with_capacity_and_hasher(cap, BuildHasherDefault::default())
}

pub fn set_with_capacity<K>(cap: usize) -> FxHashSet<K> {
  FxHashSet::with_capacity_and_hasher(cap, BuildHasherDefault::default())
}

pub fn map<K, V, const N: usize>(xs: [(K, V); N]) -> FxHashMap<K, V>
where
  K: Eq + Hash,
{
  let mut ret = map_with_capacity(xs.len());
  for (k, v) in xs {
    assert!(ret.insert(k, v).is_none());
  }
  ret
}

pub fn set<K, const N: usize>(xs: [K; N]) -> FxHashSet<K>
where
  K: Eq + Hash,
{
  let mut ret = set_with_capacity(xs.len());
  for k in xs {
    assert!(ret.insert(k));
  }
  ret
}
