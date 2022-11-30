//! Hover tests.

use crate::check::check;

#[test]
fn smoke() {
  check(
    r#"
val _ = 123
(**     ^ hover: int *)
"#,
  );
}

#[test]
fn tuple() {
  check(
    r#"
datatype 'a uh = Uh of 'a
val _ = (   3, "hi", Uh false)
(**       ^ hover: int * string * bool uh *)
"#,
  );
}

#[test]
fn fully_qualified() {
  check(
    r#"
structure Foo = struct datatype bar = baz end
val _  = Foo.baz
(**      ^ hover: Foo.bar *)
"#,
  );
}
