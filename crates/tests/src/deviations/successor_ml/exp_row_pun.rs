//! Tests for expression row punning, where `{foo}` is equivalent to `{foo = foo}`.

use crate::check::{check, check_multi, raw};

#[test]
fn default_disallow() {
  check(
    r"
fun incB {a, b, c} = {a, b = b + 1, c}
(**                   ^ expression row punning *)
",
  );
}

#[test]
fn config_allow() {
  let config = r"
version = 1
language.successor-ml.exp-row-pun = true
";
  let sml = r"
fun incB {a, b, c} = {a, b = b + 1, c}
val _ = incB
(**     ^^^^ hover: { a : ?a, b : int, c : ?b } -> { a : ?a, b : int, c : ?b } *)
";
  check_multi(raw::singleton(config, sml));
}
