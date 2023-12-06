//! Tests for ML Basis (MLB).

use crate::check::{check_bad_input, check_multi, raw};

#[test]
fn smoke_ok() {
  let s = r#"
basis A = let in B end and C = let foo.sml in D end
basis Uh = bas uh.sml end
open A C
(* hello there *)
local
  foo.sml
  bar.sml
  quz.mlb
  uh.fun
in
  structure E
  signature F = G
  functor H and I = J
end
ann "huh" in huh.sml end
signature ARRAY_UTIL
"#;
  mlb_syntax::get(s, &slash_var_path::Env::default()).unwrap();
}

#[test]
fn empty() {
  check_multi([("a.mlb", "")]);
}

#[test]
fn ident() {
  let mlb = r"
local
  a.sml
in
  structure FOO_BAR_QUZ
  signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234
end
";
  let sml = r"
structure FOO_BAR_QUZ = struct end
signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234 = sig end
";
  check_multi([("sources.mlb", mlb), ("a.sml", sml)]);
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

fn ann_diagnostics_ignore(mlb: &str, a: bool, b: bool, c: bool) {
  let reported = r#"
val () = "err"
(** + expected `unit`, found `string` *)
"#;
  let ignored = r#"
val () = "err"
"#;
  let files = [
    ("s.mlb", mlb),
    ("a.sml", if a { reported } else { ignored }),
    ("b.sml", if b { reported } else { ignored }),
    ("c.sml", if c { reported } else { ignored }),
  ];
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::None,
    min_severity: diagnostic::Severity::Error,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(files, opts);
}

#[test]
fn ann_diagnostics_ignore_all() {
  let mlb = r#"
a.sml
ann "milletDiagnosticsIgnore all" in
  b.sml
end
c.sml
"#;
  ann_diagnostics_ignore(mlb, true, false, true);
}

#[test]
fn ann_diagnostics_ignore_true() {
  let mlb = r#"
a.sml
ann "milletDiagnosticsIgnore true" in
  b.sml
end
c.sml
"#;
  ann_diagnostics_ignore(mlb, true, false, true);
}

#[test]
fn ann_diagnostics_ignore_false() {
  let mlb = r#"
ann "milletDiagnosticsIgnore true" in
  a.sml
  ann "milletDiagnosticsIgnore false" in
    b.sml
  end
end
c.sml
"#;
  ann_diagnostics_ignore(mlb, false, true, true);
}

#[test]
fn no_path() {
  check_bad_input("s.mlb", "couldn't perform file I/O", [("s.mlb", "no.mlb")]);
  cov_mark::check("no_path");
}
