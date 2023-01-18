//! Test for getting documentation on hover.

use crate::check::check;

#[test]
fn doc() {
  check(
    r#"
(*!
 * My favorite number.
 *)
val foo = 3
(** ^^^ hover: My favorite number. *)
"#,
  );
}
