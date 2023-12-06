//! Basis path variables, which are special because paths that contain them are ignored if the
//! variables are not defined in the config.

use crate::check::check_multi;

fn ignored(name: &str, contents: &str) {
  check_multi([(name, contents), ("a.sml", "val a = 3"), ("b.sml", "val b = a")]);
}

fn present(name: &str, contents: &str, config: &str) {
  check_multi([
    (config::file::PATH, config),
    (name, contents),
    ("a.sml", "val a = 3"),
    ("b.sml", "val b = a"),
  ]);
}

#[test]
fn mlb_sml_lib_ignored() {
  let mlb = r"
a.sml
$(SML_LIB)/basis.mlb
b.sml
$(SML_LIB)/foo/bar.sml
";
  ignored("s.mlb", mlb);
}

#[test]
fn cm_empty_ignored() {
  let cm = r"
Group is
  a.sml
  $/basis.cm
  b.sml
  $/foo/bar.sml
";
  ignored("s.cm", cm);
}

#[test]
fn cm_smlnj_lib_ignored() {
  let cm = r"
Group is
  a.sml
  $SMLNJ-LIB/basis.cm
  b.sml
  $SMLNJ-LIB/foo/bar.sml
";
  ignored("s.cm", cm);
}

#[test]
fn mlb_sml_lib_present() {
  let config = r#"
version = 1
workspace.path-vars.SML_LIB.value = "a"
"#;
  let mlb = r"
$(SML_LIB).sml
b.sml
";
  present("s.mlb", mlb, config);
}

#[test]
fn cm_empty_present() {
  let config = r#"
version = 1
workspace.path-vars."".value = "a"
"#;
  let cm = r"
Group is
  $.sml
  b.sml
";
  present("s.cm", cm, config);
}

#[test]
fn cm_smlnj_lib_present() {
  let config = r#"
version = 1
workspace.path-vars."SMLNJ-LIB".value = "a"
"#;
  let cm = r"
Group is
  $SMLNJ-LIB.sml
  b.sml
";
  present("s.cm", cm, config);
}
