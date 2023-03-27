//! Disallowing parts of the language.

use crate::check::{check_bad_input, check_multi, raw};

fn files<'a>(config: &'a str, sml: &'a str) -> [(&'a str, &'a str); 3] {
  [(config::file::PATH, config), ("s.mlb", "a.sml"), ("a.sml", sml)]
}

#[test]
fn exp() {
  let config = r#"
version = 1
language.exp.while = false
"#;
  let sml = r#"
val _ = 4
val _ = while true do ()
(**     ^^^^^^^^^^^^^^^^ disallowed expression: while *)
val _ = "hi"
"#;
  check_multi(files(config, sml));
}

#[test]
fn dec() {
  let config = r#"
version = 1
language.dec.signature = false
"#;
  let sml = r#"
structure Str = struct end
signature SIG = sig end
(** + disallowed declaration: signature *)
functor F() = struct end
"#;
  check_multi(files(config, sml));
}

#[test]
fn val() {
  let config = r#"
version = 1
[language.val]
"List.hd" = false
"#;
  let sml = r#"
val _ = List.hd
(**     ^^^^^^^ disallowed *)
"#;
  let opts = raw::Opts {
    std_basis: analysis::StdBasis::Full,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: diagnostic_util::Severity::Error,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(files(config, sml), opts);
}

fn with_config(config: &str) -> [(&str, &str); 2] {
  [("a.mlb", ""), (config::file::PATH, config)]
}

#[test]
fn empty_path_component() {
  let config = r#"
version = 1
[language.val]
"Foo..bar" = false
"#;
  check_bad_input(
    config::file::PATH,
    "empty string in dot-separated path: `Foo..bar`",
    with_config(config),
  );
}

#[test]
fn empty_path() {
  let config = r#"
version = 1
[language.val]
"" = false
"#;
  check_bad_input(
    config::file::PATH,
    "empty string in dot-separated path: ``",
    with_config(config),
  );
}

#[test]
fn no_such_path() {
  let config = r#"
version = 1
[language.val]
"Foo.bar" = false
"#;
  let opts = raw::Opts {
    std_basis: analysis::StdBasis::Minimal,
    // TODO remove and use `check_bad_input`
    outcome: raw::Outcome::Fail,
    limit: raw::Limit::First,
    min_severity: diagnostic_util::Severity::Warning,
    expected_input: raw::ExpectedInput::Bad { path: config::file::PATH, msg: "no such path" },
  };
  raw::get(with_config(config), opts);
}
