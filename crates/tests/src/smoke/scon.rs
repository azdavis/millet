//! Special constructors.

#![expect(clippy::needless_raw_string_hashes)]

use crate::check::check;

#[test]
fn int() {
  check(r#"val _: int = 3"#);
}

#[test]
fn neg_int() {
  check(r#"val _: int = ~4"#);
}

#[test]
fn real() {
  check(r#"val _: real = 3.2"#);
}

#[test]
fn neg_real_with_e() {
  check(r#"val _: real = ~3.2e9"#);
}

#[test]
fn neg_real_with_big_e() {
  check(r#"val _: real = 3.2E9"#);
}

#[test]
fn pos_real_with_big_neg_e() {
  check(r#"val _: real = 3.2E~9"#);
}

#[test]
fn word() {
  check(r#"val _: word = 0w123"#);
}

#[test]
fn hex_int() {
  check(r#"val _: int = 0x123beef"#);
}

#[test]
fn hex_word() {
  check(r#"val _: word = 0wx123beef"#);
}

#[test]
fn char() {
  check(r#"val _: char = #"a""#);
}

#[test]
fn string() {
  check(r#"val _: string = "foo""#);
}

/// not actually scon
#[test]
fn bool_true() {
  check(r#"val _: bool = true"#);
}

/// not actually scon
#[test]
fn bool_false() {
  check(r#"val _: bool = false"#);
}
