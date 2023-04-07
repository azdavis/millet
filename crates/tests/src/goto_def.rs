//! Tests for go to def.

use crate::check::check;

#[test]
fn val() {
  check(
    r#"
val x = 3
(** ^ def: x *)
val y = x
(**     ^ use: x *)
"#,
  );
}
