//! Tests for a leading `|` before some constructs, like the first `case` arm.

use crate::check::{check, check_multi, raw};

const ALLOW: &str = r#"
version = 1
language.successor-ml.opt-bar = true
"#;

#[test]
fn case_default_disallow() {
  check(
    r#"
  fun f x =
    case x of
    | 1 => 2
(** ^ preceding `|` *)
    | 3 => 4
    | _ => 5
"#,
  );
}

#[test]
fn case_config_allow() {
  let sml = r#"
  fun f x =
    case x of
    | 1 => 2
    | 3 => 4
    | _ => 5
"#;
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn fn_default_disallow() {
  check(
    r#"
    val f = fn
    | 1 => 2
(** ^ preceding `|` *)
    | 3 => 4
    | _ => 5
"#,
  );
}

#[test]
fn fn_config_allow() {
  let sml = r#"
    val f = fn
    | 1 => 2
    | 3 => 4
    | _ => 5
"#;
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn handle_default_disallow() {
  check(
    r#"
  exception A and B of int
  val _ = 1 handle
    | A => 2
(** ^ preceding `|` *)
    | B _ => 3
"#,
  );
}

#[test]
fn handle_config_allow() {
  let sml = r#"
  exception A and B of int
  val _ = 1 handle
    | A => 2
    | B _ => 3
"#;
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn fun_default_disallow() {
  check(
    r#"
    fun
    | f 1 = 2
(** ^ preceding `|` *)
    | f 3 = 4
    | f _ = 5
"#,
  );
}

#[test]
fn fun_config_allow() {
  let sml = r#"
    fun
    | f 1 = 2
    | f 3 = 4
    | f _ = 5
"#;
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn datatype_default_disallow() {
  check(
    r#"
    datatype t =
    | A
(** ^ preceding `|` *)
    | B of int
    | C
    | D of string
"#,
  );
}

#[test]
fn datatype_config_allow() {
  let sml = r#"
    datatype t =
    | A
    | B of int
    | C
    | D of string
"#;
  check_multi(raw::singleton(ALLOW, sml));
}
