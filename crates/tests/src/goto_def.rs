//! Tests for go to def.

use crate::check::fail;

#[test]
fn val() {
  fail(
    r#"
val x = 3
(** ^ def: x *)
val y = x
(**     ^ use: x *)
"#,
  );
}
