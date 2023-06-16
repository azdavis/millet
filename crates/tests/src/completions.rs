//! Tests for completions.

use crate::check::fail;

#[test]
fn smoke() {
  fail(
    r#"
val foo = 3

val _ =
(**   ^ completions: foo *)
"#,
  );
}

#[test]
fn in_struct() {
  fail(
    r#"
structure Foo = struct
  val bar = 3
  val quz = "hi"
end

val _ = Foo.
(**        ^ completions: bar, quz *)
"#,
  );
}
