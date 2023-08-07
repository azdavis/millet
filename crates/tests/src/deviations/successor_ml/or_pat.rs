//! Tests for allowing/disallowing or patterns.

use crate::check::{check, check_multi, raw};

#[test]
fn default_allow() {
  check(
    r#"
datatype d = A of int | B of int
fun f (A x | B x) = x
"#,
  );
}

#[test]
fn config_allow() {
  let config = r#"
version = 1
language.successor-ml.or-pat = true
"#;
  let sml = r#"
datatype d = A of int | B of int
fun f (A x | B x) = x
"#;
  check_multi(raw::singleton(config, sml));
}

#[test]
fn config_disallow() {
  let config = r#"
version = 1
language.successor-ml.or-pat = false
"#;
  let sml = r#"
datatype d = A of int | B of int
fun f (A x | B x) = x
(**    ^^^^^^^^^ disallowed Successor ML feature: or patterns *)
"#;
  check_multi(raw::singleton(config, sml));
}
