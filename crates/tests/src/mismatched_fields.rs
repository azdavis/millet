//! Tests when two record/tuple types have mismatched fields.

use crate::check::check;

#[test]
fn missing_1() {
  check(
    r"
val a : { x : int, y: string } = { x = 3 }
(** + missing 1 field: `y` *)
",
  );
}

#[test]
fn extra_1() {
  check(
    r#"
val a : { x : int, y: string } = { x = 3, y = "hi", z = false }
(** + has 1 extra field: `z` *)
"#,
  );
}

#[test]
fn both_1() {
  check(
    r#"
val a : { x : int, y: string } = { y = "hi", w = false }
(** + missing 1 field: `x`, and has 1 extra field: `w` *)
"#,
  );
}

#[test]
fn missing_2() {
  check(
    r"
val a : { x : int, y: string } = ()
(** + missing 2 fields: `x`, `y` *)
",
  );
}

#[test]
fn extra_2() {
  check(
    r#"
val a : { x : int, y: string } = { x = 3, y = "hi", z = false, w = 1.1 }
(** + has 2 extra fields: `w`, `z` *)
"#,
  );
}

#[test]
fn both_2() {
  check(
    r"
val a : { x : int, y: string } = { z = false, w = 1.1 }
(** + missing 2 fields: `x`, `y`, and has 2 extra fields: `w`, `z` *)
",
  );
}
