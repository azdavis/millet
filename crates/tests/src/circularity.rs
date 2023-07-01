//! Circularity, aka the occurs check.

use crate::check::check;

#[test]
fn return_self() {
  check(
    r#"
    fun f _ = f
(** ^^^^^^^^^^^ circular type: `?a` occurs in `_ -> ?a` *)
"#,
  );
}

#[test]
fn apply_self() {
  check(
    r#"
fun f x = x x
(**       ^ circular type: `?a` occurs in `?a -> ?b` *)
"#,
  );
}
