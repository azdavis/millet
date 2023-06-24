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

val f = foo
(**     ^^^ hover: Some docs. *)
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

val f = foo
(**     ^^^ hover: Some docs. *)
"#,
  );
}

#[test]
fn typ() {
  check(
    r#"
(*!
 * Some docs.
 *)
type t = int
(**  ^ hover: Some docs. *)

type u = t
(**      ^ hover: Some docs. *)
"#,
  );
}

#[test]
fn datatype() {
  check(
    r#"
(*!
 * Some docs.
 *)
datatype d = D
(**      ^ hover: Some docs. *)

type u = d
(**      ^ hover: Some docs. *)
"#,
  );
}

#[test]
fn exception() {
  check(
    r#"
(*!
 * Some docs.
 *)
exception E
(**       ^ hover: Some docs. *)

val e = E
(**     ^ hover: Some docs. *)
"#,
  );
}

#[test]
fn exception_copy() {
  fail(
    r#"
(*!
 * Some docs.
 *)
exception E
(**       ^ hover: Some docs. *)

exception e = E
(**           ^ hover: Some docs. *)
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

#[test]
fn std_basis_structure() {
  check_with_std_basis(
    r#"
structure L = List
(**           ^^^^ hover: a collection of utility functions for manipulating polymorphic lists *)
"#,
  );
}
