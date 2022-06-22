use crate::check::{check, fail};

#[test]
fn multi_rest() {
  check(
    r#"
val _ = fn {a, ..., ...} => 3
(**        ^^^^^^^^^^^^^ cannot have multiple `...` *)
"#,
  );
}

#[test]
fn rest_not_last() {
  check(
    r#"
val _ = fn {..., a} => 3
(**        ^^^^^^^^ `...` must come last *)
"#,
  );
}

#[test]
fn selector_smoke() {
  fail(
    r#"
val _ : int = #1 (3, "hi")
"#,
  );
}

#[test]
fn as_rest() {
  fail(
    r#"
fun combine {a, b} = a + b
val f = fn x as {b, ...} => combine x + 4 + b
"#,
  );
}
