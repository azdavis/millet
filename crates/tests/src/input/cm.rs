//! Tests for SML/NJ Compilation Manager (CM).

mod syntax;

use crate::check::{check_bad_input, check_multi};

pub(crate) const EMPTY: &str = "Group is";

#[test]
fn empty() {
  check_multi([("a.cm", EMPTY)]);
}

#[test]
fn ident() {
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
fn union() {
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
fn intersection() {
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
fn difference() {
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
fn source_file() {
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
fn source_dash() {
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
fn source_not_in_files() {
  let cm = r#"
Library
  source(foo.sml)
is
  bar.sml
"#;
  check_bad_input("a.cm", "not in file list", [("a.cm", cm), ("foo.sml", ""), ("bar.sml", "")]);
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
fn group_undef_path_var() {
  let config = r#"
version = 1
workspace.root = "a.cm"
"#;
  check_bad_input(
    "a.cm",
    "undefined path variable: `BAD`",
    [
      (config::file::PATH, config),
      ("a.cm", "Library group($BAD.cm) is b.cm"),
      ("b.cm", "Group is"),
    ],
  );
}

#[test]
fn no_path() {
  check_bad_input("s.cm", "couldn't perform file I/O", [("s.cm", "Group is no.cm")]);
}

#[test]
fn empty_library_err() {
  check_bad_input(
    "a.cm",
    "invalid empty export for `Library`",
    [("a.cm", "Library is a.sml"), ("a.sml", "structure S = struct end")],
  );
}

// TODO should export all
#[test]
fn empty_group_exports_none() {
  let config = r#"
version = 1
workspace.root = "b.cm"
"#;
  let b = r#"
val _ = S.x + 2
(**     ^^^ undefined structure: `S` *)
"#;
  check_multi([
    ("a.cm", "Group is a.sml"),
    ("a.sml", "structure S = struct val x = 3 end"),
    ("b.cm", "Group is a.cm b.sml"),
    ("b.sml", b),
    (config::file::PATH, config),
  ]);
}
