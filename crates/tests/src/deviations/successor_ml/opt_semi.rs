//! Tests for a trailing `;` at the end of a `let` expression sequence.

use crate::check::{check, check_multi, raw};

#[test]
fn e1_default_disallow() {
  check(
    r#"
val _ = let in 1; end
(**             ^ trailing `;` *)
"#,
  );
}

#[test]
fn e2_default_disallow() {
  check(
    r#"
val _ = let in 1; 2; end
(**                ^ trailing `;` *)
"#,
  );
}

#[test]
fn config_allow() {
  let config = r#"
version = 1
language.successor-ml.opt-semi = true
"#;
  let sml = r#"
val () = let in 1; end
val () = let in 1; 2; end
"#;
  check_multi(raw::singleton(config, sml));
}

#[test]
fn unit_ty() {
  let config = r#"
version = 1
language.successor-ml.opt-semi = true
"#;
  let sml = r#"
val x = let in 1; end
fun oop (y: int) = y + 1
val _ = oop x
(**         ^ expected `int`, found `unit` *)
"#;
  check_multi(raw::singleton(config, sml));
}
