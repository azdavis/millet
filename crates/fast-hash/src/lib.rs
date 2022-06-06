//! A thin wrapper over [`rustc_hash`] with some extra helper functions.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use std::hash::{BuildHasherDefault, Hash};

pub use rustc_hash::{FxHashMap, FxHashSet};

/// Returns a map with the given capacity.
pub fn map_with_capacity<K, V>(cap: usize) -> FxHashMap<K, V> {
  FxHashMap::with_capacity_and_hasher(cap, BuildHasherDefault::default())
}

/// Returns a set with the given capacity.
pub fn set_with_capacity<K>(cap: usize) -> FxHashSet<K> {
  FxHashSet::with_capacity_and_hasher(cap, BuildHasherDefault::default())
}

/// Returns a map with the given elements.
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

/// Returns a set with the given elements.
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
