//! Types concerning SML infix operators, precedence, and associativity.

use fast_hash::{map_with_capacity, FxHashMap};
use once_cell::sync::Lazy;

/// A mapping from names to (in)fixities.
pub type Env = FxHashMap<str_util::Name, Infix>;

/// The default infix operators in the std basis.
pub static STD_BASIS: Lazy<Env> = Lazy::new(|| {
  let ops_arr: [(Infix, &[&str]); 6] = [
    (Infix::left(7), &["*", "/", "div", "mod"]),
    (Infix::left(6), &["+", "-", "^"]),
    (Infix::right(5), &["::", "@"]),
    (Infix::left(4), &["=", "<>", ">", ">=", "<", "<="]),
    (Infix::left(3), &[":=", "o"]),
    (Infix::left(0), &["before"]),
  ];
  let mut ret = map_with_capacity(ops_arr.iter().map(|(_, names)| names.len()).sum());
  for (info, names) in ops_arr {
    for &name in names {
      ret.insert(str_util::Name::new(name), info);
    }
  }
  ret
});

/// Information about an infix name.
#[derive(Debug, Clone, Copy)]
pub struct Infix {
  /// The precedence.
  pub prec: u16,
  /// The associativity.
  pub assoc: Assoc,
}

impl Infix {
  /// Returns a new `Infix` with left associativity.
  #[must_use]
  pub fn left(prec: u16) -> Self {
    Self { prec, assoc: Assoc::Left }
  }

  /// Returns a new `Infix` with right associativity.
  #[must_use]
  pub fn right(prec: u16) -> Self {
    Self { prec, assoc: Assoc::Right }
  }
}

/// Associativity for infix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
  /// `infix`
  Left,
  /// `infixr`
  Right,
}
