//! Tests to make sure Millet behaves as expected on the public documentation.

use crate::{check::markdown::check, repo::root_dir};

#[test]
fn diagnostics() {
  for entry in std::fs::read_dir(root_dir().join("docs").join("diagnostics")).unwrap() {
    let entry = entry.unwrap();
    let path = entry.path();
    let contents = std::fs::read_to_string(&path).unwrap();
    check(&contents);
  }
}

#[test]
fn primitives() {
  check(include_str!("../../../docs/primitives.md"));
}

#[test]
fn tokens() {
  check(include_str!("../../../docs/tokens.md"));
}
