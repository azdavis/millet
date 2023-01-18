//! Test for getting documentation on hover.

use crate::check::{check, fail};

#[test]
fn val() {
  check(
    r#"
(*!
 * Some docs.
 *)
val foo = 3
(** ^^^ hover: Some docs. *)
"#,
  );
}

#[test]
fn fun() {
  check(
    r#"
(*!
 * Some docs.
 *)
fun foo () = ()
(** ^^^ hover: Some docs. *)
"#,
  );
}

#[test]
fn typ() {
  fail(
    r#"
(*!
 * Some docs.
 *)
type t = int
(**  ^ hover: Some docs. *)
"#,
  );
}

#[test]
fn datatype() {
  fail(
    r#"
(*!
 * Some docs.
 *)
datatype d = D
(**      ^ hover: Some docs. *)
"#,
  );
}

#[test]
fn exception() {
  fail(
    r#"
(*!
 * Some docs.
 *)
exception E
(**       ^ hover: Some docs. *)
"#,
  );
}
