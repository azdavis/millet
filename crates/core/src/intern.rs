//! String interning.

use maplit::hashmap;
use std::collections::HashMap;

/// A reference to a string. To learn what string this represents, you must ask the StrStore created
/// from the StrStoreMut that returned this StrRef to you.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StrRef(usize);

// TODO generate this with a macro?
impl StrRef {
  // unit, char, exn
  pub const UNIT: Self = Self(0);
  pub const CHAR: Self = Self(1);
  pub const EXN: Self = Self(2);
  // bool
  pub const BOOL: Self = Self(3);
  pub const TRUE: Self = Self(4);
  pub const FALSE: Self = Self(5);
  pub const NOT: Self = Self(6);
  // string
  pub const STRING: Self = Self(7);
  pub const CARAT: Self = Self(8);
  // word, int
  pub const WORD: Self = Self(9);
  pub const INT: Self = Self(10);
  pub const TILDE: Self = Self(11);
  pub const PLUS: Self = Self(12);
  pub const MINUS: Self = Self(13);
  pub const STAR: Self = Self(14);
  pub const DIV: Self = Self(15);
  pub const MOD: Self = Self(16);
  // real
  pub const REAL: Self = Self(17);
  pub const SLASH: Self = Self(18);
  // order
  pub const ORDER: Self = Self(19);
  pub const LESS: Self = Self(20);
  pub const EQUAL: Self = Self(21);
  pub const GREATER: Self = Self(22);
  // comparison
  pub const EQ: Self = Self(23);
  pub const NEQ: Self = Self(24);
  pub const GT: Self = Self(25);
  pub const GT_EQ: Self = Self(26);
  pub const LT: Self = Self(27);
  pub const LT_EQ: Self = Self(28);
  // option
  pub const OPTION: Self = Self(29);
  pub const SOME: Self = Self(30);
  pub const NONE: Self = Self(31);
  // list
  pub const LIST: Self = Self(32);
  pub const NIL: Self = Self(33);
  pub const CONS: Self = Self(34);
  pub const AT: Self = Self(35);
  // ref
  pub const REF: Self = Self(36);
  pub const ASSIGN: Self = Self(37);
  // exceptions
  pub const MATCH: Self = Self(38);
  pub const BIND: Self = Self(39);
}

/// A mutable factory of StrRefs. Allows creating new StrRefs from Strings.
pub struct StrStoreMut {
  store: HashMap<String, StrRef>,
  next: usize,
}

impl StrStoreMut {
  #[allow(clippy::new_without_default)]
  /// Returns an new StrStoreMut containing only the special StrRefs.
  pub fn new() -> Self {
    let s = String::from;
    let store = hashmap![
      // unit, char, exn
      s("unit") => StrRef::UNIT,
      s("char") => StrRef::CHAR,
      s("exn") => StrRef::EXN,
      // bool
      s("bool") => StrRef::BOOL,
      s("true") => StrRef::TRUE,
      s("false") => StrRef::FALSE,
      s("not") => StrRef::NOT,
      // string
      s("string") => StrRef::STRING,
      s("^") => StrRef::CARAT,
      // word, int
      s("word") => StrRef::WORD,
      s("int") => StrRef::INT,
      s("~") => StrRef::TILDE,
      s("+") => StrRef::PLUS,
      s("-") => StrRef::MINUS,
      s("*") => StrRef::STAR,
      s("div") => StrRef::DIV,
      s("mod") => StrRef::MOD,
      // real
      s("real") => StrRef::REAL,
      s("/") => StrRef::SLASH,
      // order
      s("order") => StrRef::ORDER,
      s("LESS") => StrRef::LESS,
      s("EQUAL") => StrRef::EQUAL,
      s("GREATER") => StrRef::GREATER,
      // comparison
      s("=") => StrRef::EQ,
      s("<>") => StrRef::NEQ,
      s("<") => StrRef::LT,
      s("<=") => StrRef::LT_EQ,
      s(">") => StrRef::GT,
      s(">=") => StrRef::GT_EQ,
      // option
      s("option") => StrRef::OPTION,
      s("SOME") => StrRef::SOME,
      s("NONE") => StrRef::NONE,
      // list
      s("list") => StrRef::LIST,
      s("nil") => StrRef::NIL,
      s("::") => StrRef::CONS,
      s("@") => StrRef::AT,
      // ref
      s("ref") => StrRef::REF,
      s(":=") => StrRef::ASSIGN,
      // exceptions
      s("Match") => StrRef::MATCH,
      s("Bind") => StrRef::BIND,
    ];
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
