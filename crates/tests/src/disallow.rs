//! Disallowing parts of the language.

use crate::check::{check_bad_input, check_multi, raw};

fn empty(config: &str) -> [(&str, &str); 2] {
  [("a.mlb", ""), (config::file::PATH, config)]
}

fn singleton<'a>(config: &'a str, sml: &'a str) -> [(&'a str, &'a str); 3] {
  [(config::file::PATH, config), ("s.mlb", "a.sml"), ("a.sml", sml)]
}

fn multi_std_basis<const N: usize>(outcome: raw::Outcome, singleton: [(&str, &str); N]) {
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Full,
    outcome,
    limit: raw::Limit::First,
    min_severity: diagnostic::Severity::Warning,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(singleton, opts);
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
(**     ^^^^^^^^^^^^^^^^ disallowed expression: `while` *)
val _ = "hi"
"#;
  check_multi(singleton(config, sml));
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
(** + disallowed declaration: `signature` *)
functor F() = struct end
"#;
  check_multi(singleton(config, sml));
}

#[test]
fn val_smoke() {
  let config = r#"
version = 1
[language.val]
"List.tabulate" = false
"#;
  let sml = r#"
val tab = List.tabulate
(**       ^^^^^^^^^^^^^ disallowed value: `tabulate` *)
"#;
  multi_std_basis(raw::Outcome::Pass, singleton(config, sml));
}

#[test]
fn val_open() {
  let config = r#"
version = 1
[language.val]
"List.tabulate" = false
"#;
  let sml = r#"
local
  open List
in
  val tab = tabulate
(**         ^^^^^^^^ disallowed value: `tabulate` *)
end
"#;
  multi_std_basis(raw::Outcome::Pass, singleton(config, sml));
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
    empty(config),
  );
}

#[test]
fn empty_path() {
  let config = r#"
version = 1
[language.val]
"" = true
"#;
  check_bad_input(config::file::PATH, "empty string in dot-separated path: ``", empty(config));
}

#[test]
fn no_such_path() {
  let config = r#"
version = 1
[language.val]
"Foo.bar" = false
"#;
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Fail,
    limit: raw::Limit::First,
    min_severity: diagnostic::Severity::Warning,
    expected_input: raw::ExpectedInput::Bad { path: config::file::PATH, msg: "no such path" },
  };
  raw::get(empty(config), opts);
}

#[test]
fn disallow_fqn_disallows_alias() {
  let config = r#"
version = 1
[language.val]
"List.hd" = false
"#;
  let sml = r#"
val h = hd
(**     ^^ disallowed value: `hd` *)
"#;
  multi_std_basis(raw::Outcome::Fail, singleton(config, sml));
}

#[test]
fn disallow_alias_disallows_fqn() {
  let config = r#"
version = 1
[language.val]
"hd" = false
"#;
  let sml = r#"
val h = List.hd
(**     ^^^^^^^ disallowed value: `hd` *)
"#;
  multi_std_basis(raw::Outcome::Fail, singleton(config, sml));
}

const LIST_SHADOW: &str = r#"
structure List = struct
  val hd = 3
end
val n = List.hd + 4
"#;

#[test]
fn shadow_fqn() {
  let config = r#"
version = 1
[language.val]
"List.hd" = false
"#;
  multi_std_basis(raw::Outcome::Pass, singleton(config, LIST_SHADOW));
}

#[test]
fn shadow_alias() {
  let config = r#"
version = 1
[language.val]
"hd" = false
"#;
  let sml = r#"
val hd = 3
val n = hd + 4
"#;
  multi_std_basis(raw::Outcome::Pass, singleton(config, sml));
}

#[test]
fn shadow_multi_file() {
  let config = r#"
version = 1
[language.val]
"List.hd" = false
"#;
  let a = r#"
structure List = struct
  val hd = 3
end
"#;
  let b = r#"
val n = List.hd + 4
"#;
  multi_std_basis(
    raw::Outcome::Pass,
    [(config::file::PATH, config), ("s.mlb", "a.sml b.sml"), ("a.sml", a), ("b.sml", b)],
  );
}

fn no_list(sml: &str) {
  let config = r#"
version = 1
[language.structure]
"List" = false
"#;
  multi_std_basis(raw::Outcome::Pass, singleton(config, sml));
}

#[test]
fn str_smoke() {
  let sml = r#"
structure L = List
(**           ^^^^ disallowed structure: `List` *)
"#;
  no_list(sml);
}

#[test]
fn str_val() {
  let sml = r#"
val tab = List.tabulate
(**       ^^^^^^^^^^^^^ disallowed structure: `List` *)
"#;
  no_list(sml);
}

#[test]
fn str_open() {
  let sml = r#"
structure S = struct
  open List
(**  + disallowed structure: `List` *)
end
"#;
  no_list(sml);
}

#[test]
fn str_type() {
  let sml = r#"
type 'a seq = 'a List.list
(**           ^^^^^^^^^^^^ disallowed structure: `List` *)
"#;
  no_list(sml);
}

#[test]
fn str_exn() {
  let sml = r#"
exception Sub = List.Subscript
(** + disallowed structure: `List` *)
"#;
  no_list(sml);
}

#[test]
fn str_shadow() {
  no_list(LIST_SHADOW);
}
