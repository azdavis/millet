//! A map optimized for the use-case of cloning and adding a few elements to the clone.

use fast_hash::{map, FxHashMap, FxHashSet};
use std::borrow::Borrow;
use std::hash::Hash;

#[cfg(not(feature = "sync"))]
type Rc<T> = std::rc::Rc<T>;
#[cfg(feature = "sync")]
type Rc<T> = std::sync::Arc<T>;

/// A chain map.
#[derive(Debug, Clone)]
pub struct ChainMap<K, V> {
  stack: Vec<Rc<FxHashMap<K, V>>>,
}

impl<K, V> ChainMap<K, V> {
  /// Returns whether this is empty.
  #[must_use]
  pub fn is_empty(&self) -> bool {
    self.stack.iter().all(|x| x.is_empty())
  }

  /// Append other onto self, emptying other.
  pub fn append(&mut self, other: &mut Self) {
    self.stack.append(&mut other.stack);
  }
}

impl<K, V> ChainMap<K, V>
where
  K: Hash + Eq,
{
  /// Inserts a new key-value pair into the map. Returns whether the value was newly inserted.
  pub fn insert(&mut self, k: K, v: V) -> bool {
    let is_new = self.stack.iter().all(|m| !m.contains_key(&k));
    self.stack.push(Rc::new(map([(k, v)])));
    is_new
  }

  /// Gets the value at the key.
  pub fn get<Q>(&self, k: &Q) -> Option<&V>
  where
    Q: ?Sized + Hash + Eq,
    K: Borrow<Q>,
  {
    self.stack.iter().rev().find_map(|m| m.get(k))
  }

  /// Returns an iterator over the keys and values.
  pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
    let is_len_one = self.stack.len() == 1;
    let mut iter = self.stack.iter().rev().flat_map(|x| x.iter());
    let mut seen = FxHashSet::<&K>::default();
    std::iter::from_fn(move || loop {
      let (k, v) = iter.next()?;
      if is_len_one || seen.insert(k) {
        return Some((k, v));
      }
    })
  }
}

impl<K, V> ChainMap<K, V>
where
  K: Hash + Eq + Clone,
  V: Clone,
{
  /// Gets the mutable value at the key.
  pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
  where
    Q: ?Sized + Hash + Eq,
    K: Borrow<Q>,
  {
    self.consolidate_().get_mut(k)
  }

  /// Removes the value at the key. Returns the old value.
  pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
  where
    Q: ?Sized + Hash + Eq,
    K: Borrow<Q>,
  {
    self.consolidate_().remove(k)
  }

  /// Returns an iterator over the keys and mutable values.
  pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
    self.consolidate_().iter_mut()
  }

  /// Returns a draining iterator over the keys and values.
  pub fn drain(&mut self) -> impl Iterator<Item = (K, V)> + '_ {
    self.consolidate_().drain()
  }

  /// Consolidates this to make it faster to clone next time.
  ///
  /// This itself might be pretty expensive.
  pub fn consolidate(&mut self) {
    self.consolidate_();
  }

  fn consolidate_(&mut self) -> &mut FxHashMap<K, V> {
    if self.stack.len() != 1 {
      let mut one = FxHashMap::<K, V>::default();
      for m in self.stack.drain(..) {
        let m = Rc::try_unwrap(m).unwrap_or_else(|m| (*m).clone());
        one.extend(m);
      }
      self.stack.push(Rc::new(one));
    }
    Rc::make_mut(self.stack.first_mut().unwrap())
  }
}

impl<K, V> Default for ChainMap<K, V> {
  fn default() -> Self {
    Self { stack: Vec::new() }
  }
}

impl<K, V> Extend<(K, V)> for ChainMap<K, V>
where
  K: Hash + Eq,
{
  fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
    let top: FxHashMap<_, _> = iter.into_iter().collect();
    self.stack.push(Rc::new(top));
  }
}

impl<K, V> FromIterator<(K, V)> for ChainMap<K, V>
where
  K: Hash + Eq,
{
  fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
    let mut ret = Self::default();
    ret.extend(iter);
    ret
  }
}

impl<K, V, const N: usize> From<[(K, V); N]> for ChainMap<K, V>
where
  K: Hash + Eq,
{
  fn from(value: [(K, V); N]) -> Self {
    value.into_iter().collect()
  }
}

impl<K, V> IntoIterator for ChainMap<K, V>
where
  K: Hash + Eq + Clone,
  V: Clone,
{
  type Item = (K, V);

  type IntoIter = IntoIter<K, V>;

  fn into_iter(mut self) -> Self::IntoIter {
    IntoIter(std::mem::take(self.consolidate_()).into_iter())
  }
}

/// The return of `into_iter()`.
#[derive(Debug)]
pub struct IntoIter<K, V>(std::collections::hash_map::IntoIter<K, V>);

impl<K, V> Iterator for IntoIter<K, V> {
  type Item = (K, V);

  fn next(&mut self) -> Option<Self::Item> {
    self.0.next()
  }
}
