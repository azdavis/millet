//! Tests to make sure Millet behaves as expected on the public documentation.

use crate::check::markdown::check;

#[test]
fn primitives() {
  check(include_str!("../../../docs/primitives.md"));
}

#[test]
fn tokens() {
  check(include_str!("../../../docs/tokens.md"));
}
