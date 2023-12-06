//! Basis path variables, which are special because paths that contain them are ignored if the
//! variables are not defined in the config.

use crate::check::check_multi;

fn ignored(name: &str, contents: &str) {
  check_multi([(name, contents), ("a.sml", "val a = 3"), ("b.sml", "val b = a")]);
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
