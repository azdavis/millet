use crate::check::check;

#[test]
fn selector() {
  check(
    r#"
val _: int = #1 (3, "hi")
(**          ^^ unsupported language construct: `...` pattern rows *)
"#,
  );
}
