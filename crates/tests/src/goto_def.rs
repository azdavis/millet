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

#[test]
fn fun() {
  check(
    r#"
fun foo () = ()
(** ^^^^^^^^^^^ def: foo *)
val y = foo
(**     ^^^ use: foo *)
"#,
  );
}

#[test]
fn type_() {
  check(
    r#"
type bar = int
(** + def: bar *)
val y : bar = 3
(**     ^^^ use: bar *)
"#,
  );
}
