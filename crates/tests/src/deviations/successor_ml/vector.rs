//! Vector expressions and patterns like `#[a, b]`.

use crate::check::{check, check_multi, raw};

#[test]
fn exp_default_disallow() {
  check(
    r#"
val _ = #[1, 2]
(**     ^^^^^^^ vector expressions *)
"#,
  );
}

#[test]
fn pat_default_disallow() {
  check(
    r#"
val _ = fn #[x, 2] => x | _ => 2
(**        ^^^^^^^ vector patterns *)
"#,
  );
}

#[test]
fn exp_config_allow() {
  let config = r#"
version = 1
language.successor-ml.vector = true
"#;
  let sml = r#"
val x = #[1, 2]
val () = x
(** + expected `unit`, found `int vector` *)
"#;
  check_multi(raw::singleton(config, sml));
}
