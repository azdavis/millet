//! Warnings about unused items.

use crate::check::check_with_warnings;

#[test]
fn smoke() {
  check_with_warnings(
    r#"
fun f x = ()
(**   ^ unused value: x *)
"#,
  );
}
