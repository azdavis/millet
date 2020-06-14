//! String interning.

use maplit::hashmap;
use std::collections::HashMap;

/// A reference to a string. To learn what string this represents, you must ask the StrStore created
/// from the StrStoreMut that returned this StrRef to you.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StrRef(usize);

// TODO generate this with a macro?
impl StrRef {
  pub const STAR: StrRef = StrRef(0);
  pub const UNIT: StrRef = StrRef(1);
  pub const BOOL: StrRef = StrRef(2);
  pub const INT: StrRef = StrRef(3);
  pub const REAL: StrRef = StrRef(4);
  pub const WORD: StrRef = StrRef(5);
  pub const CHAR: StrRef = StrRef(6);
  pub const STRING: StrRef = StrRef(7);
  pub const OPTION: StrRef = StrRef(8);
  pub const LIST: StrRef = StrRef(9);
  pub const REF: StrRef = StrRef(10);
  pub const TRUE: StrRef = StrRef(11);
  pub const FALSE: StrRef = StrRef(12);
  pub const SOME: StrRef = StrRef(13);
  pub const NONE: StrRef = StrRef(14);
  pub const NIL: StrRef = StrRef(15);
  pub const CONS: StrRef = StrRef(16);
  pub const PLUS: StrRef = StrRef(17);
  pub const MINUS: StrRef = StrRef(18);
  pub const SLASH: StrRef = StrRef(19);
  pub const CARAT: StrRef = StrRef(20);
  pub const AT: StrRef = StrRef(21);
  pub const NOT: StrRef = StrRef(22);
  pub const DIV: StrRef = StrRef(23);
  pub const LT: StrRef = StrRef(24);
  pub const LT_EQ: StrRef = StrRef(25);
  pub const GT: StrRef = StrRef(26);
  pub const GT_EQ: StrRef = StrRef(27);
  pub const NEQ: StrRef = StrRef(28);
  pub const TILDE: StrRef = StrRef(29);
  pub const ORDER: StrRef = StrRef(30);
  pub const LESS: StrRef = StrRef(31);
  pub const EQUAL: StrRef = StrRef(32);
  pub const GREATER: StrRef = StrRef(33);
  pub const EQ: StrRef = StrRef(34);
  pub const ASSIGN: StrRef = StrRef(35);
}

/// A mutable factory of StrRefs. Allows creating new StrRefs from Strings.
pub struct StrStoreMut {
  store: HashMap<String, StrRef>,
  next: usize,
}

impl StrStoreMut {
  /// Returns an new StrStoreMut containing only the special StrRefs.
  pub fn new() -> Self {
    let store = hashmap! {
      "*".to_owned() => StrRef::STAR,
      "unit".to_owned() => StrRef::UNIT,
      "bool".to_owned() => StrRef::BOOL,
      "int".to_owned() => StrRef::INT,
      "real".to_owned() => StrRef::REAL,
      "word".to_owned() => StrRef::WORD,
      "char".to_owned() => StrRef::CHAR,
      "string".to_owned() => StrRef::STRING,
      "option".to_owned() => StrRef::OPTION,
      "list".to_owned() => StrRef::LIST,
      "ref".to_owned() => StrRef::REF,
      "true".to_owned() => StrRef::TRUE,
      "false".to_owned() => StrRef::FALSE,
      "SOME".to_owned() => StrRef::SOME,
      "NONE".to_owned() => StrRef::NONE,
      "nil".to_owned() => StrRef::NIL,
      "::".to_owned() => StrRef::CONS,
      "+".to_owned() => StrRef::PLUS,
      "-".to_owned() => StrRef::MINUS,
      "/".to_owned() => StrRef::SLASH,
      "^".to_owned() => StrRef::CARAT,
      "@".to_owned() => StrRef::AT,
      "not".to_owned() => StrRef::NOT,
      "div".to_owned() => StrRef::DIV,
      "<".to_owned() => StrRef::LT,
      "<=".to_owned() => StrRef::LT_EQ,
      ">".to_owned() => StrRef::GT,
      ">=".to_owned() => StrRef::GT_EQ,
      "<>".to_owned() => StrRef::NEQ,
      "~".to_owned() => StrRef::TILDE,
      "order".to_owned() => StrRef::ORDER,
      "LESS".to_owned() => StrRef::LESS,
      "EQUAL".to_owned() => StrRef::EQUAL,
      "GREATER".to_owned() => StrRef::GREATER,
      "=".to_owned() => StrRef::EQ,
      ":=".to_owned() => StrRef::ASSIGN,
    };
    Self {
      next: store.len(),
      store,
    }
  }

  /// Inserts a string slice into this StrStoreMut. Returns an StrRef corresponding to that string.
  /// Converts the string slice to an owned String iff this string slice was not already present.
  pub fn insert_str(&mut self, s: &str) -> StrRef {
    if let Some(&id) = self.store.get(s) {
      return id;
    }
    let ret = StrRef(self.next);
    self.store.insert(s.to_owned(), ret);
    self.next += 1;
    ret
  }

  /// Same as insert, but better if you already have ownership of the string.
  pub fn insert_string(&mut self, s: String) -> StrRef {
    if let Some(&id) = self.store.get(&s) {
      return id;
    }
    let ret = StrRef(self.next);
    self.store.insert(s, ret);
    self.next += 1;
    ret
  }

  /// Converts this StrStoreMut into an StrStore, preventing further mutation.
  pub fn finish(self) -> StrStore {
    let mut store = vec![String::new(); self.store.len()];
    for (s, id) in self.store {
      // each index should be assigned exactly once, based on the way we handed out StrRefs.
      store[id.0] = s;
    }
    StrStore { store }
  }
}

/// An immutable store of Strings. Allows looking up the String corresponding to a StrRef.
pub struct StrStore {
  store: Vec<String>,
}

impl StrStore {
  /// Returns the string slice corresponding to this StrRef.
  pub fn get(&self, id: StrRef) -> &str {
    self.store[id.0].as_str()
  }
}
