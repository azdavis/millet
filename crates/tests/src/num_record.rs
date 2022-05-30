use crate::check::{check, fail};

#[test]
fn t_01() {
  check(
    r#"
type t = { 1: int, 2: bool }
    val _: t = ()
(** ^^^^^^^^^^^^^ mismatched types: expected int * bool, found unit *)
"#,
  );
}

#[test]
fn t_02() {
  fail(
    r#"
type t = { 0: int, 1: bool }
(**        ^ expected a label *)
"#,
  );
}

#[test]
fn t_03() {
  check(
    r#"
type t = { 1: int }
    val _: t = ()
(** ^^^^^^^^^^^^^ mismatched types: expected { 1 : int }, found unit *)
"#,
  );
}

#[test]
fn t_04() {
  check(
    r#"
type t = { 1: int, 3: bool }
    val _: t = ()
(** ^^^^^^^^^^^^^ mismatched types: expected { 1 : int, 3 : bool }, found unit *)
"#,
  );
}
