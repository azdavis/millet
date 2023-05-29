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
fn fixity_across_files_default_off() {
  let config = r#"
version = 1
"#;
  let uses_infix_bad = r#"
fun (a %%% b) = a + b
(** + non-infix name used as infix: `%%%` *)
"#;
  check_multi([
    (config::file::PATH, config),
    ("s.mlb", "a.sml b.sml"),
    ("a.sml", "infix %%%"),
    ("b.sml", uses_infix_bad),
  ]);
}

#[test]
fn fixity_across_files_config_off() {
  let config = r#"
version = 1
[language]
fixity-across-files = false
"#;
  let uses_infix_bad = r#"
fun (a %%% b) = a + b
(** + non-infix name used as infix: `%%%` *)
"#;
  check_multi([
    (config::file::PATH, config),
    ("s.mlb", "a.sml b.sml"),
    ("a.sml", "infix %%%"),
    ("b.sml", uses_infix_bad),
  ]);
}

#[test]
fn fixity_across_files_config_on() {
  let config = r#"
version = 1
[language]
fixity-across-files = true
"#;
  let uses_infix_good = r#"
fun (a %%% b) = a + b
"#;
  check_multi([
    (config::file::PATH, config),
    ("s.mlb", "a.sml b.sml"),
    ("a.sml", "infix %%%"),
    ("b.sml", uses_infix_good),
  ]);
}
