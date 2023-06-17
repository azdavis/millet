//! Test for getting documentation on hover.

use crate::check::{check, check_with_std_basis, fail};

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

#[test]
fn fun_doc_usage() {
  check(
    r#"
(*!
 * Returns the number incremented.
 *)
fun inc x = x + 1

val _ = inc
(**     ^^^ hover: Returns the number incremented. *)
"#,
  );
}

#[test]
fn primitive() {
  check(
    r#"
val _ = false
(**     ^^^^^ hover: represents logical falsity *)
"#,
  );
  cov_mark::check("primitive_doc");
}

#[test]
fn std_basis() {
  check_with_std_basis(
    r#"
val _ = List.Empty
(**          ^^^^^ hover: indicates that an empty list was given as an argument *)
"#,
  );
}
