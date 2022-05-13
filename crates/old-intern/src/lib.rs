//! String interning.
//!
//! There are many times where we want to store or compare user-written strings, for instance when
//! working with identifiers (which we do a lot). It should be efficient, i.e. O(1), to duplicate a
//! string and to compare strings for equality.
//!
//! However, the String type is heap-allocated, which means duplicating it results in a new heap
//! allocation. Further, comparing Strings for equality is O(min(len(s1), len(s2))).
//!
//! To solve this, we "intern" all user-written strings. When we see a new user-written String, we
//! generate a new StrRef for it and return that StrRef. If we later see the same String, we return
//! the same StrRef. Then, string comparison is just seeing if the IDs are equal, which is O(1)
//! since the IDs are just integers. Duplicating a string is just duplicating its StrRef, which is
//! again O(1).
//!
//! But, we may want to later actually display the string referenced by an ID, for instance in an
//! error message. For that, we must look up the String referenced by the StrRef.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use rustc_hash::FxHashMap;
use std::fmt;

/// A reference to a string. To learn what string this represents, you must ask the StrStore created
/// from the StrStoreMut that returned this StrRef to you.
///
/// NOTE that the PartialOrd and Ord implementations are based not on the ordering of the strings
/// represented by a StrRef, but by the internal IDs which are handed out according to source order.
/// We only use the ordering of StrRefs to sort record labels. It might be better to pass in a
/// &StrStore to the place where we need to do that so we can sort the labels by the actual strings.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct StrRef(usize);

const SPECIAL_STR_REF: usize = 41;

impl fmt::Debug for StrRef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.0 < SPECIAL_STR_REF {
      write!(f, "StrRef(special: {})", self.0)
    } else {
      write!(f, "StrRef(regular: {})", self.0 - SPECIAL_STR_REF)
    }
  }
}

/// New StrRefs should be appended to the bottom. This can help avoid big diffs.
#[allow(missing_docs)]
impl StrRef {
  pub const UNIT: Self = Self(0);
  pub const CHAR: Self = Self(1);
  pub const EXN: Self = Self(2);
  pub const BOOL: Self = Self(3);
  pub const TRUE: Self = Self(4);
  pub const FALSE: Self = Self(5);
  pub const NOT: Self = Self(6);
  pub const STRING: Self = Self(7);
  pub const CARAT: Self = Self(8);
  pub const WORD: Self = Self(9);
  pub const INT: Self = Self(10);
  pub const TILDE: Self = Self(11);
  pub const PLUS: Self = Self(12);
  pub const MINUS: Self = Self(13);
  pub const STAR: Self = Self(14);
  pub const DIV: Self = Self(15);
  pub const MOD: Self = Self(16);
  pub const REAL: Self = Self(17);
  pub const SLASH: Self = Self(18);
  pub const ORDER: Self = Self(19);
  pub const LESS: Self = Self(20);
  pub const EQUAL: Self = Self(21);
  pub const GREATER: Self = Self(22);
  pub const EQ: Self = Self(23);
  pub const NEQ: Self = Self(24);
  pub const GT: Self = Self(25);
  pub const GT_EQ: Self = Self(26);
  pub const LT: Self = Self(27);
  pub const LT_EQ: Self = Self(28);
  pub const OPTION: Self = Self(29);
  pub const SOME: Self = Self(30);
  pub const NONE: Self = Self(31);
  pub const LIST: Self = Self(32);
  pub const NIL: Self = Self(33);
  pub const CONS: Self = Self(34);
  pub const AT: Self = Self(35);
  pub const REF: Self = Self(36);
  pub const ASSIGN: Self = Self(37);
  pub const MATCH: Self = Self(38);
  pub const BIND: Self = Self(39);
  pub const ABS: Self = Self(40);
}

/// A mutable factory of StrRefs. Allows creating new StrRefs from Strings.
#[derive(Debug)]
pub struct StrStoreMut {
  store: FxHashMap<String, StrRef>,
  next: usize,
}

impl StrStoreMut {
  /// Returns an new StrStoreMut containing only the special StrRefs.
  pub fn new() -> Self {
    let mut store = FxHashMap::with_capacity_and_hasher(
      SPECIAL_STR_REF,
      std::hash::BuildHasherDefault::default(),
    );
    macro_rules! ins {
      ($s:expr, $name:ident) => {
        assert!(store.insert($s.to_owned(), StrRef::$name).is_none());
      };
    }
    ins!("unit", UNIT);
    ins!("char", CHAR);
    ins!("exn", EXN);
    ins!("bool", BOOL);
    ins!("true", TRUE);
    ins!("false", FALSE);
    ins!("not", NOT);
    ins!("string", STRING);
    ins!("^", CARAT);
    ins!("word", WORD);
    ins!("int", INT);
    ins!("~", TILDE);
    ins!("+", PLUS);
    ins!("-", MINUS);
    ins!("*", STAR);
    ins!("div", DIV);
    ins!("mod", MOD);
    ins!("real", REAL);
    ins!("/", SLASH);
    ins!("order", ORDER);
    ins!("LESS", LESS);
    ins!("EQUAL", EQUAL);
    ins!("GREATER", GREATER);
    ins!("=", EQ);
    ins!("<>", NEQ);
    ins!("<", LT);
    ins!("<=", LT_EQ);
    ins!(">", GT);
    ins!(">=", GT_EQ);
    ins!("option", OPTION);
    ins!("SOME", SOME);
    ins!("NONE", NONE);
    ins!("list", LIST);
    ins!("nil", NIL);
    ins!("::", CONS);
    ins!("@", AT);
    ins!("ref", REF);
    ins!(":=", ASSIGN);
    ins!("Match", MATCH);
    ins!("Bind", BIND);
    ins!("abs", ABS);
    assert_eq!(store.len(), SPECIAL_STR_REF);
    Self {
      next: SPECIAL_STR_REF,
      store,
    }
  }

  /// Inserts a string into this StrStoreMut. Returns an StrRef corresponding to that string.
  pub fn insert(&mut self, s: std::borrow::Cow<'_, str>) -> StrRef {
    if let Some(&id) = self.store.get(&*s) {
      return id;
    }
    let ret = StrRef(self.next);
    self.store.insert(s.into_owned(), ret);
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
    for s in store.iter() {
      assert!(!s.is_empty());
    }
    StrStore { store }
  }
}

impl Default for StrStoreMut {
  fn default() -> Self {
    Self::new()
  }
}

/// An immutable store of Strings. Allows looking up the String corresponding to a StrRef.
#[derive(Debug)]
pub struct StrStore {
  store: Vec<String>,
}

impl StrStore {
  /// Returns the string slice corresponding to this StrRef.
  pub fn get(&self, id: StrRef) -> &str {
    self
      .store
      .get(id.0)
      .expect("gave a StrStore a StrRef that didn't come from its StrStoreMut")
      .as_str()
  }
}
