//! Identifiers.

use std::collections::HashMap;

/// A reference to an identifier in source code. To learn what string this Ident represents, you
/// must ask the IdentStore created from the IdentMaker that returned this Ident to you.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Ident(usize);

// TODO generate this with a macro?
const STAR: Ident = Ident(0);

impl Ident {
  /// Returns whether this is the special identifier '*'.
  pub fn is_star(&self) -> bool {
    *self == STAR
  }
}

/// A mutable factory of Idents. Allows creating new Idents from Strings.
pub struct IdentMaker {
  store: HashMap<String, Ident>,
  next: usize,
}

impl IdentMaker {
  /// Returns an empty IdentMaker.
  pub fn new() -> Self {
    let mut store = HashMap::new();
    store.insert("*".to_owned(), STAR);
    Self {
      next: store.len(),
      store,
    }
  }

  /// Inserts a string into this IdentMaker. Returns an Ident corresponding to that string.
  pub fn insert(&mut self, s: String) -> Ident {
    if let Some(&id) = self.store.get(&s) {
      return id;
    }
    let ret = Ident(self.next);
    self.store.insert(s, ret);
    self.next += 1;
    ret
  }

  /// Converts this IdentMaker into an IdentStore, preventing further mutation.
  pub fn into_store(self) -> IdentStore {
    let mut store = vec![String::new(); self.store.len()];
    for (s, id) in self.store {
      store[id.0] = s;
    }
    IdentStore { store }
  }
}

/// An immutable store of Idents. Allows looking up the String corresponding to an Ident.
pub struct IdentStore {
  store: Vec<String>,
}

impl IdentStore {
  /// Returns the string slice corresponding to this Ident.
  pub fn get(&self, id: Ident) -> &str {
    self.store[id.0].as_str()
  }
}
