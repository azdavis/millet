//! The Lunar ML ES import syntax.

use crate::check::{check, raw};

#[track_caller]
fn check_ok(s: &str) {
  let config = r"
version = 1
language.lunar-ml.es-import = true
";
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: diagnostic::Severity::Warning,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(raw::singleton(config, s), opts);
}

#[test]
fn err_no_pats() {
  check(
    r#"
    _esImport "foo"
(** ^^^^^^^^^^^^^^^ disallowed declaration: `_esImport` *)
    val x = 1 + 2
"#,
  );
}

#[test]
fn ok_no_pats() {
  check_ok(
    r#"
    _esImport "foo"

    val x = 1 + 2
"#,
  );
}

#[test]
fn err_default() {
  check(
    r#"
    _esImport func from "foo"
(** ^^^^^^^^^^^^^^^^^^^^^^^^^ disallowed declaration: `_esImport` *)
    val () = func ()
"#,
  );
}

#[test]
fn ok_default() {
  check_ok(
    r#"
    _esImport func from "foo"

    val () = func ()
"#,
  );
}

#[test]
fn err_default_pure() {
  check(
    r#"
    _esImport [pure] func from "foo"
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ disallowed declaration: `_esImport` *)
    val () = func ()
"#,
  );
}

#[test]
fn ok_default_pure() {
  check_ok(
    r#"
    _esImport [pure] func from "foo"

    val () = func ()
"#,
  );
}

#[test]
fn err_default_and_more() {
  check(
    r#"
    _esImport func, { foo, bar as barr, "fun" as fun' } from "foo"
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ disallowed declaration: `_esImport` *)
    val () = func ()
    val () = barr ()
    val () = fun' ()
"#,
  );
}

#[test]
fn ok_default_and_more() {
  check_ok(
    r#"
    _esImport func, { foo, bar as barr, "fun" as fun' } from "foo"

    val () = func ()
    val () = barr ()
    val () = fun' ()
"#,
  );
}

#[test]
fn err_with_ty_annot() {
  check(
    r#"
    _esImport { foo : int -> unit } from "foo"
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ disallowed declaration: `_esImport` *)
    val () = foo 3
    (**      ^^^ hover: int -> unit *)
"#,
  );
}

#[test]
fn ok_with_ty_annot() {
  check_ok(
    r#"
    _esImport { foo : int -> unit } from "foo"

    val () = foo 3
    (**      ^^^ hover: int -> unit *)
"#,
  );
}

#[test]
fn no_name_for_str() {
  check_ok(
    r#"
_esImport { "foo" } from "bar"
(**         ^^^^^ no `as` for an ES import spec with a string name *)
"#,
  );
}
