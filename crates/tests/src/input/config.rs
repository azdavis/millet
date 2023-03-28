//! Tests focusing on the Millet config file.

use crate::check::{check_bad_input, check_multi};
use crate::input::cm;

const EMPTY: &str = "version = 1";

#[test]
fn no_root_group_in_config_err() {
  check_bad_input("", "and no `workspace.root` glob pattern", [(config::file::PATH, EMPTY)]);
}

#[test]
fn no_root_group_in_config_ok() {
  check_multi([("a.cm", cm::EMPTY), (config::file::PATH, EMPTY)]);
}

#[test]
fn multiple_root_groups_ok() {
  let config = r#"
version = 1
[workspace]
# prefer foo over bar
root = "a.cm"
"#;
  check_multi([("a.cm", cm::EMPTY), ("b.cm", cm::EMPTY), (config::file::PATH, config)]);
}

#[test]
fn mlb_cm_config_cm_ok() {
  let config = r#"
version = 1
[workspace]
root = "a.cm"
"#;
  check_multi([("a.cm", cm::EMPTY), ("a.mlb", ""), (config::file::PATH, config)]);
}

#[test]
fn mlb_cm_config_mlb_ok() {
  let config = r#"
version = 1
workspace.root = "a.mlb"
"#;
  check_multi([("a.cm", cm::EMPTY), ("a.mlb", ""), (config::file::PATH, config)]);
}

#[test]
fn invalid_version() {
  check_bad_input(
    config::file::PATH,
    "invalid config version",
    [(config::file::PATH, "version = 123")],
  );
}

#[test]
fn path_not_exist() {
  let config = r#"
version = 1
workspace.root = "nope.cm"
"#;
  check_bad_input(
    config::file::PATH,
    "glob pattern matched no paths:",
    [(config::file::PATH, config)],
  );
}

#[test]
fn parse_err() {
  check_bad_input(
    config::file::PATH,
    "couldn't parse config",
    [(config::file::PATH, "岡部倫太郎")],
  );
}

#[test]
fn cycle() {
  let config = r#"
version = 1
[workspace]
root = "a.cm"
  "#;
  check_bad_input(
    "b.cm",
    "there is a cycle",
    [("a.cm", "Group is b.cm"), ("b.cm", "Group is a.cm"), (config::file::PATH, config)],
  );
}

#[test]
fn glob_not_group() {
  let config = r#"
version = 1
workspace.root = "nope.txt"
"#;
  check_bad_input(
    config::file::PATH,
    "pattern matched no paths",
    [("a.cm", cm::EMPTY), (config::file::PATH, config)],
  );
}

#[test]
fn unknown_property() {
  let config = r#"
version = 1
foo = "bar"
workspace.woofer = "bark.txt"
[quz]
chihiro = true
"#;
  check_multi([("a.cm", cm::EMPTY), (config::file::PATH, config)]);
}

#[test]
fn path_vars_ok() {
  let config = r#"
version = 1
[workspace.path-vars]
okabe = { value = "rintarou" }
shiina = { path = "mayuri" }
hashida = { workspace-path = "itaru" }
"#;
  check_multi([("a.cm", cm::EMPTY), (config::file::PATH, config)]);
}

#[test]
fn path_vars_err() {
  let config = r#"
version = 1
[workspace.path-vars]
makise = { christina = "kurisu" }
"#;
  check_bad_input(
    config::file::PATH,
    "unknown variant `christina`",
    [("a.cm", cm::EMPTY), (config::file::PATH, config)],
  );
}

#[test]
fn diagnostics_severity_ok() {
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
fn diagnostics_severity_unknown() {
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
  let sml = r#"
val _ = (1)
(**     ^^^ unnecessary parentheses *)
"#;
  check_multi([(config::file::PATH, config), ("a.mlb", "a.sml"), ("a.sml", sml)]);
}
