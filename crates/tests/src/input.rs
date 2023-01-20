//! Low-level tests for analysis input.

mod cm;
mod mlb;
mod slash_var_path;

use crate::check::{check_bad_input, check_multi};

const EMPTY_CM: &str = "Group is";

#[test]
fn arbitrary_root_group() {
  check_multi([("a.cm", EMPTY_CM)]);
}

#[test]
fn no_root_group_empty() {
  check_bad_input("", "no *.cm, *.mlb", []);
}

#[test]
fn no_root_group_empty_millet_toml() {
  check_bad_input("", "no `workspace.root` specified", [(config::FILE_NAME, "version = 1")]);
}

#[test]
fn no_root_group_wrong_ext() {
  check_bad_input("", "no *.cm, *.mlb", [("foo.txt", "hi there"), ("foo.rs", "fn main() {}")]);
}

#[test]
fn multiple_root_groups_err() {
  check_bad_input("b.cm", "multiple root group files", [("a.cm", EMPTY_CM), ("b.cm", EMPTY_CM)]);
}

#[test]
fn multiple_root_groups_ok() {
  let config = r#"
version = 1
[workspace]
# prefer foo over bar
root = "a.cm"
"#;
  check_multi([("a.cm", EMPTY_CM), ("b.cm", EMPTY_CM), (config::FILE_NAME, config)]);
}

#[test]
fn no_root_group_in_config_ok() {
  check_multi([("a.cm", EMPTY_CM), (config::FILE_NAME, "version = 1")]);
}

#[test]
fn config_invalid_version() {
  check_bad_input(
    config::FILE_NAME,
    "invalid config version",
    [(config::FILE_NAME, "version = 123")],
  );
}

#[test]
fn config_path_not_exist() {
  let config = r#"
version = 1
workspace.root = "nope.cm"
"#;
  check_bad_input(config::FILE_NAME, "pattern matched no paths:", [(config::FILE_NAME, config)]);
}

#[test]
fn config_parse_err() {
  check_bad_input(config::FILE_NAME, "unexpected character", [(config::FILE_NAME, "岡部倫太郎")]);
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
    [("a.cm", "Group is b.cm"), ("b.cm", "Group is a.cm"), (config::FILE_NAME, config)],
  );
}

#[test]
fn not_group() {
  let config = r#"
version = 1
workspace.root = "nope.txt"
"#;
  check_bad_input(
    config::FILE_NAME,
    "pattern matched no paths",
    [("a.cm", EMPTY_CM), (config::FILE_NAME, config)],
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
  check_multi([("a.cm", EMPTY_CM), (config::FILE_NAME, config)]);
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
  check_multi([("a.cm", EMPTY_CM), (config::FILE_NAME, config)]);
}

#[test]
fn path_vars_err() {
  let config = r#"
version = 1
[workspace.path-vars]
makise = { christina = "kurisu" }
"#;
  check_bad_input(
    config::FILE_NAME,
    "unknown variant `christina`",
    [("a.cm", EMPTY_CM), (config::FILE_NAME, config)],
  );
}

#[test]
fn mlb() {
  check_multi([("a.mlb", "")]);
}

#[test]
fn mlb_cm_err() {
  check_bad_input("a.cm", "multiple root group files", [("a.cm", EMPTY_CM), ("a.mlb", "")]);
}

#[test]
fn mlb_cm_config_cm_ok() {
  let config = r#"
version = 1
[workspace]
root = "a.cm"
"#;
  check_multi([("a.cm", EMPTY_CM), ("a.mlb", ""), (config::FILE_NAME, config)]);
}

#[test]
fn mlb_cm_config_mlb_ok() {
  let config = r#"
version = 1
workspace.root = "a.mlb"
"#;
  check_multi([("a.cm", EMPTY_CM), ("a.mlb", ""), (config::FILE_NAME, config)]);
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
  check_multi([("a.mlb", ""), (config::FILE_NAME, config)]);
}

#[test]
fn diagnostics_severity_unknown() {
  let config = r#"
version = 1
[diagnostics]
1001.severity = "Warning"
"#;
  check_bad_input(
    config::FILE_NAME,
    "unknown variant `Warning`",
    [("a.mlb", ""), (config::FILE_NAME, config)],
  );
}

#[test]
fn std_basis_export() {
  let cm = r#"
Library
  library($/basis.cm)
is
  $/basis.cm
"#;
  check_multi([("a.cm", cm)]);
}

#[test]
fn std_basis_group() {
  let cm = r#"
Library
  group($/basis.cm)
is
  $/basis.cm
"#;
  check_bad_input("a.cm", "expected a regular path or `-`", [("a.cm", cm)]);
}

#[test]
fn cm_ident() {
  let cm = r#"
Library
  structure FOO_BAR_QUZ
  signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234
is
  a.sml
"#;
  let sml = r#"
structure FOO_BAR_QUZ = struct end
signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234 = sig end
"#;
  check_multi([("a.cm", cm), ("a.sml", sml)]);
}

#[test]
fn mlb_ident() {
  let mlb = r#"
local
  a.sml
in
  structure FOO_BAR_QUZ
  signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234
end
"#;
  let sml = r#"
structure FOO_BAR_QUZ = struct end
signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234 = sig end
"#;
  check_multi([("sources.mlb", mlb), ("a.sml", sml)]);
}

#[test]
fn cm_union() {
  let cm = r#"
Library
  structure FOO
  (
    signature BAR
    signature QUZ
  )
is
  a.sml
"#;
  let sml = r#"
structure FOO = struct end
signature BAR = sig end
signature QUZ = sig end
"#;
  check_multi([("a.cm", cm), ("a.sml", sml)]);
}

#[test]
fn cm_intersection() {
  let cm = r#"
Library
  structure FOO * signature BAR
is
  a.sml
"#;
  let sml = r#"
structure FOO = struct end
signature BAR = sig end
"#;
  check_multi([("a.cm", cm), ("a.sml", sml)]);
}

#[test]
fn cm_difference() {
  let cm = r#"
Library
  structure FOO - signature BAR
is
  a.sml
"#;
  let sml = r#"
structure FOO = struct end
signature BAR = sig end
"#;
  check_multi([("a.cm", cm), ("a.sml", sml)]);
}

#[test]
fn cm_source_file() {
  let cm = r#"
Library
  source(foo.sml)
is
  foo.sml
  bar.sml
"#;
  check_multi([("a.cm", cm), ("foo.sml", ""), ("bar.sml", "")]);
}

#[test]
fn cm_source_dash() {
  let cm = r#"
Library
  source(-)
is
  foo.sml
  bar.sml
"#;
  check_multi([("a.cm", cm), ("foo.sml", ""), ("bar.sml", "")]);
}

#[test]
fn cm_source_not_in_files() {
  let cm = r#"
Library
  source(foo.sml)
is
  bar.sml
"#;
  check_bad_input("a.cm", "not in file list", [("a.cm", cm), ("foo.sml", ""), ("bar.sml", "")]);
}

#[test]
fn multi_ann() {
  let mlb = r#"
ann
  "foo bar"
  "baz"
in
end
"#;
  check_multi([("s.mlb", mlb)]);
}
