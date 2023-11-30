//! Vector expressions and patterns like `#[a, b]`.

use crate::check::{check, check_multi, raw};

#[test]
fn exp_default_disallow() {
  check(
    r"
val _ = #[1, 2]
(**     ^^^^^^^ vector expressions *)
",
  );
}

#[test]
fn pat_default_disallow() {
  check(
    r"
val _ = fn #[x, 2] => x | _ => 2
(**        ^^^^^^^ vector patterns *)
",
  );
}

const ALLOW: &str = r"
version = 1
language.successor-ml.vector = true
";

#[test]
fn exp_config_allow() {
  let sml = r"
val x = #[1, 2]
val () = x
(** + expected `unit`, found `int vector` *)
";
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_config_allow() {
  let sml = r#"
fun classify xs =
(** ^^^^^^^^ hover: ?a vector -> string *)
  case xs of
    #[] => "empty"
  | #[_, _, _] => "triple"
  | #[_, _] => "pair"
  | #[_] => "singleton"
  | _ => "many"
"#;
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_match_0_0_wild() {
  let sml = r"
fun f xs =
  case xs of
    #[] => 1
  | #[] => 2
(** ^^^ unreachable pattern *)
  | _ => 3
";
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_match_1_1_wild() {
  let sml = r"
fun f xs =
  case xs of
    #[_] => 1
  | #[_] => 2
(** + unreachable pattern *)
  | _ => 3
";
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_match_0() {
  let sml = r"
fun f xs =
  case xs of
(** + missing `_` *)
    #[] => 1
";
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_match_1() {
  let sml = r"
fun f xs =
  case xs of
(** + missing `_` *)
    #[_] => 1
";
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_match_0_1_wild() {
  let sml = r"
fun f xs =
  case xs of
    #[] => 1
  | #[_] => 2
  | _ => 3
";
  check_multi(raw::singleton(ALLOW, sml));
}

#[test]
fn pat_match_0_2() {
  let sml = r"
fun f xs =
  case xs of
(** + missing `_` *)
    #[] => 1
  | #[_, _] => 2
";
  check_multi(raw::singleton(ALLOW, sml));
}
