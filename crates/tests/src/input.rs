//! Low-level tests for analysis input.

mod cm;
mod mlb;
mod slash_var_path;

use crate::check::{check_bad_input, check_multi};

#[test]
fn no_root_group_empty() {
  check_bad_input("", "no *.cm, *.mlb", []);
}

#[test]
fn no_root_group_empty_millet_toml() {
  check_bad_input(
    "",
    "and no `workspace.root` glob pattern",
    [(config::file::NAME, "version = 1")],
  );
}

#[test]
fn no_root_group_wrong_ext() {
  check_bad_input("", "no *.cm, *.mlb", [("foo.txt", "hi there"), ("foo.rs", "fn main() {}")]);
}

#[test]
fn multiple_root_groups_err() {
  check_bad_input(
    "b.cm",
    "multiple *.cm or *.mlb files",
    [("a.cm", cm::EMPTY), ("b.cm", cm::EMPTY)],
  );
}

#[test]
fn multiple_root_groups_ok() {
  let config = r#"
version = 1
[workspace]
# prefer foo over bar
root = "a.cm"
"#;
  check_multi([("a.cm", cm::EMPTY), ("b.cm", cm::EMPTY), (config::file::NAME, config)]);
}

#[test]
fn no_root_group_in_config_ok() {
  check_multi([("a.cm", cm::EMPTY), (config::file::NAME, "version = 1")]);
}

#[test]
fn config_invalid_version() {
  check_bad_input(
    config::file::NAME,
    "invalid config version",
    [(config::file::NAME, "version = 123")],
  );
}

#[test]
fn config_path_not_exist() {
  let config = r#"
version = 1
workspace.root = "nope.cm"
"#;
  check_bad_input(
    config::file::NAME,
    "glob pattern matched no paths:",
    [(config::file::NAME, config)],
  );
}

#[test]
fn config_parse_err() {
  check_bad_input(
    config::file::NAME,
    "couldn't parse config",
    [(config::file::NAME, "岡部倫太郎")],
  );
}

#[test]
fn cycle_1() {
  check_bad_input("a.cm", "there is a cycle", [("a.cm", "Group is a.cm")]);
}

#[test]
fn cycle_2() {
  let config = r#"
version = 1
[workspace]
root = "a.cm"
  "#;
  check_bad_input(
    "b.cm",
    "there is a cycle",
    [("a.cm", "Group is b.cm"), ("b.cm", "Group is a.cm"), (config::file::NAME, config)],
  );
}

#[test]
fn not_group() {
  let config = r#"
version = 1
workspace.root = "nope.txt"
"#;
  check_bad_input(
    config::file::NAME,
    "pattern matched no paths",
    [("a.cm", cm::EMPTY), (config::file::NAME, config)],
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
  check_multi([("a.cm", cm::EMPTY), (config::file::NAME, config)]);
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
  check_multi([("a.cm", cm::EMPTY), (config::file::NAME, config)]);
}

#[test]
fn path_vars_err() {
  let config = r#"
version = 1
[workspace.path-vars]
makise = { christina = "kurisu" }
"#;
  check_bad_input(
    config::file::NAME,
    "unknown variant `christina`",
    [("a.cm", cm::EMPTY), (config::file::NAME, config)],
  );
}

#[test]
fn mlb_cm_err() {
  check_bad_input("a.cm", "multiple *.cm or *.mlb files", [("a.cm", cm::EMPTY), ("a.mlb", "")]);
}

#[test]
fn mlb_cm_config_cm_ok() {
  let config = r#"
version = 1
[workspace]
root = "a.cm"
"#;
  check_multi([("a.cm", cm::EMPTY), ("a.mlb", ""), (config::file::NAME, config)]);
}

#[test]
fn mlb_cm_config_mlb_ok() {
  let config = r#"
version = 1
workspace.root = "a.mlb"
"#;
  check_multi([("a.cm", cm::EMPTY), ("a.mlb", ""), (config::file::NAME, config)]);
}

#[test]
fn diagnostics_severity() {
  let config = r#"
version = 1
[diagnostics]
1001.severity = "error"
1002.severity = "warning"
1003.severity = "ignore"
"#;
  check_multi([("a.mlb", ""), (config::file::NAME, config)]);
}

#[test]
fn diagnostics_severity_unknown() {
  let config = r#"
version = 1
[diagnostics]
1001.severity = "Warning"
"#;
  check_bad_input(
    config::file::NAME,
    "unknown variant `Warning`",
    [("a.mlb", ""), (config::file::NAME, config)],
  );
}

#[test]
fn undefined_path_var() {
  check_bad_input("s.mlb", "undefined path variable: FOO", [("s.mlb", "$(FOO).sml")]);
}
