//! Tests for `do e`, which is the same as `val () = e`.

use crate::check::{check, check_multi, raw};

#[test]
fn default_disallow() {
  check(
    r#"
    fun print _ = ()
    do print "hi"
(** ^^^^^^^^^^^^^ disallowed *)
"#,
  );
}

#[test]
fn config_allow() {
  let config = r"
version = 1
language.successor-ml.do-dec = true
";
  let sml = r#"
fun print _ = ()
do print "hi"

val x =
  let
    val y = 4
    do print "hi... "
    val z = y + 2
    do print "bye"
  in
    y * z * z
  end

do ()
"#;
  check_multi(raw::singleton(config, sml));
}

#[test]
fn exp_unit_ty() {
  let config = r"
version = 1
language.successor-ml.do-dec = true
";
  let sml = r#"
do "hi"
(** + expected `unit`, found `string` *)
"#;
  check_multi(raw::singleton(config, sml));
}
