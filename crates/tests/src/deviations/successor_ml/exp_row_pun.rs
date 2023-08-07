//! Tests for expression row punning, where `{foo}` is equivalent to `{foo = foo}`.

use crate::check::check;

#[test]
fn disallow() {
  check(
    r#"
fun incB r =
  case r of {a, b, c} => {a, b = b + 1, c}
(**                       ^ unsupported: expression row punning *)
"#,
  );
}
