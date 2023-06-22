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
