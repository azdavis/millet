//! "Well-known" types that are available at the top level and should be reported unqualified.
//!
//! Tests should:
//!
//! 1. Cause a type error and assert that the type is reported unqualified.
//! 2. Contain a witness to the fact that the type is really available unqualified.

use crate::check::check_with_std_basis;

#[test]
fn option() {
  check_with_std_basis(
    r"
val () = SOME ()
(** + expected `unit`, found `unit option` *)

type 'a witness = 'a option
",
  );
}

#[test]
fn array() {
  check_with_std_basis(
    r"
val () = Array.fromList [()]
(** + expected `unit`, found `unit array` *)

type 'a witness = 'a array
",
  );
}

#[test]
fn vector() {
  check_with_std_basis(
    r"
val () = Vector.fromList [()]
(** + expected `unit`, found `unit vector` *)

type 'a witness = 'a vector
",
  );
}
