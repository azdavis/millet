//! Misc input tests.

use crate::{check::check_bad_input, input::cm};

#[test]
fn no_root_group_empty() {
  check_bad_input("", "no *.cm, *.mlb", []);
}

#[test]
fn no_root_group_wrong_ext() {
  check_bad_input("", "no *.cm, *.mlb", [("foo.txt", "hey")]);
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
fn self_cycle() {
  check_bad_input("a.cm", "there is a cycle", [("a.cm", "Group is a.cm")]);
}

#[test]
fn mlb_cm_err() {
  check_bad_input("a.cm", "multiple *.cm or *.mlb files", [("a.cm", cm::EMPTY), ("a.mlb", "")]);
}

#[test]
fn undefined_path_var_root() {
  check_bad_input("s.mlb", "undefined path variable: `FOO`", [("s.mlb", "$(FOO).sml")]);
}

#[test]
fn undefined_path_var_import() {
  let config = r#"
version = 1
workspace.root = "a.mlb"
"#;
  check_bad_input(
    "b.mlb",
    "undefined path variable: `BAR`",
    [(config::file::PATH, config), ("a.mlb", "b.mlb"), ("b.mlb", "$(BAR).sml")],
  );
  cov_mark::check("undefined_path_var_import");
}
