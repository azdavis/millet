//! Identifiers.

use std::collections::HashMap;

/// A reference to an identifier in source code. To learn what string this Ident represents, you
/// must ask the IdentStore created from the IdentMaker that returned this Ident to you.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Ident(usize);

// TODO generate this with a macro?
impl Ident {
  pub const STAR: Ident = Ident(0);
  pub const INT: Ident = Ident(1);
  pub const REAL: Ident = Ident(2);
  pub const WORD: Ident = Ident(3);
  pub const CHAR: Ident = Ident(4);
  pub const STRING: Ident = Ident(5);
  pub const LIST: Ident = Ident(6);
  pub const NIL: Ident = Ident(7);
  pub const CONS: Ident = Ident(8);
  pub const TRUE: Ident = Ident(9);
  pub const FALSE: Ident = Ident(10);
}

/// A mutable factory of Idents. Allows creating new Idents from Strings.
pub struct IdentMaker {
  store: HashMap<String, Ident>,
  next: usize,
}

impl IdentMaker {
  /// Returns an empty IdentMaker.
  pub fn new() -> Self {
    let mut store = HashMap::with_capacity(11);
    store.insert("*".to_owned(), Ident::STAR);
    store.insert("int".to_owned(), Ident::INT);
    store.insert("real".to_owned(), Ident::REAL);
    store.insert("word".to_owned(), Ident::WORD);
    store.insert("char".to_owned(), Ident::CHAR);
    store.insert("string".to_owned(), Ident::STRING);
    store.insert("list".to_owned(), Ident::LIST);
    store.insert("nil".to_owned(), Ident::NIL);
    store.insert("::".to_owned(), Ident::CONS);
    store.insert("true".to_owned(), Ident::TRUE);
    store.insert("false".to_owned(), Ident::FALSE);
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
