//! Errors about re-binding special names.

use crate::check::check;

#[test]
fn apply_eq() {
  check(
    r#"
fun apply_eq (op =) a b = a = b
(** + cannot re-bind name: = *)
"#,
  );
}

#[test]
fn ref_ctor() {
  check(
    r#"
datatype no = ref
(** + cannot re-bind name: ref *)
"#,
  );
}

#[test]
fn val_it() {
  check(
    r#"
val it = 3
(** + cannot re-bind name: it *)
"#,
  );
}
