//! Tests for record update, like `{ a where b = 3 }`.

use crate::check::{check, check_multi, raw};

#[test]
fn default_disallow() {
  check(
    r#"
val x = { a = 1, b = "hey" }
val y = { x where b = "hi" }
(**       ^^^^^^^ record update *)
"#,
  );
}

fn check_allow(sml: &str) {
  let config = r"
version = 1
language.successor-ml.record-update = true
";
  check_multi(raw::singleton(config, sml));
}

#[test]
fn smoke() {
  check_allow(
    r#"
val x = { a = 1, b = "hey" }
val y = { x where b = "hi" }
(** ^ hover: { a : int, b : string } *)
"#,
  );
}

#[test]
fn change_type() {
  check_allow(
    r#"
val x = { a = 1, b = "hey" }
val y = { x where b = false }
(** ^ hover: { a : int, b : bool } *)
"#,
  );
}
