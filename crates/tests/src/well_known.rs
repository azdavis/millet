//! "Well-known" types that are available at the top level and should be reported unqualified.

use crate::check::check_with_std_basis;

#[test]
fn option() {
  check_with_std_basis(
    r#"
val () = SOME ()
(** + expected `unit`, found `unit option` *)
"#,
  );
}

#[test]
fn array() {
  check_with_std_basis(
    r#"
val () = Array.array (1, ())
(** + expected `unit`, found `unit array` *)
"#,
  );
}

#[test]
fn vector() {
  check_with_std_basis(
    r#"
val () = Vector.fromList [()]
(** + expected `unit`, found `unit vector` *)
"#,
  );
}
