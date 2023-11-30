//! Tests for config for diagnostics.

use crate::check::{check_bad_input, check_multi};

#[test]
fn ok() {
  let config = r#"
version = 1
[diagnostics]
1001.severity = "error"
1002.severity = "warning"
1003.severity = "ignore"
"#;
  check_multi([("a.mlb", ""), (config::file::PATH, config)]);
}

#[test]
fn unknown() {
  let config = r#"
version = 1
[diagnostics]
1001.severity = "Warning"
"#;
  check_bad_input(
    config::file::PATH,
    "unknown variant `Warning`",
    [("a.mlb", ""), (config::file::PATH, config)],
  );
}

#[test]
fn ignore_op_andalso() {
  let config = r#"
version = 1
[diagnostics]
4011.severity = "ignore"
"#;
  check_multi([(config::file::PATH, config), ("a.mlb", "a.sml"), ("a.sml", "val _ = op andalso")]);
}

#[test]
fn ignore_mlb_undefined() {
  let config = r#"
version = 1
[diagnostics]
1017.severity = "ignore"
"#;
  check_multi([(config::file::PATH, config), ("a.mlb", "structure Foo")]);
}

#[test]
fn error_paren() {
  let config = r#"
version = 1
[diagnostics]
4014.severity = "error"
"#;
  let sml = r"
val _ = (1)
(**     ^^^ unnecessary parentheses *)
";
  check_multi([(config::file::PATH, config), ("a.mlb", "a.sml"), ("a.sml", sml)]);
}
