//! Misc input tests.

use crate::{check::check_bad_input, input::cm};

#[test]
fn no_root_group_empty() {
  check_bad_input("", "no *.cm, *.mlb", []);
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
fn self_cycle() {
  check_bad_input("a.cm", "there is a cycle", [("a.cm", "Group is a.cm")]);
}

#[test]
fn mlb_cm_err() {
  check_bad_input("a.cm", "multiple *.cm or *.mlb files", [("a.cm", cm::EMPTY), ("a.mlb", "")]);
}

#[test]
fn undefined_path_var() {
  check_bad_input("s.mlb", "undefined path variable: FOO", [("s.mlb", "$(FOO).sml")]);
}
