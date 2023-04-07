//! Tests for forbidding opaque ascription.

use crate::check::check;

#[test]
fn pat() {
  check(
    r#"
val x :> int = 3
(**   ^^ not allowed here *)
"#,
  );
}

#[test]
fn exp() {
  check(
    r#"
val x = 3 :> int
(**       ^^ not allowed here *)
"#,
  );
}

#[test]
fn fun_ret() {
  check(
    r#"
fun f x :> int = 3
(**     ^^ not allowed here *)
"#,
  );
}

#[test]
fn ty_row() {
  check(
    r#"
type foo = {
  x : int,
  y :> string
(** ^^ not allowed here *)
}
"#,
  );
}

#[test]
fn lab_row() {
  check(
    r#"
fun f {a :> int as b} = a + b
(**      ^^ not allowed here *)
"#,
  );
}
