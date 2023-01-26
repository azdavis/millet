//! The `use` builtin.

use crate::check::check_with_warnings;

#[test]
fn literal() {
  check_with_warnings(
    r#"
val () = use "foo.sml"
(**      ^^^^^^^^^^^^^ no additional definitions from "foo.sml" brought into scope *)
"#,
  );
}

#[test]
fn non_literal() {
  check_with_warnings(
    r#"
val s = "bar.sml"
val () = use s
(**      ^^^^^ no additional definitions brought into scope *)
"#,
  );
}
