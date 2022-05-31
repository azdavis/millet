use crate::check::check;

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
  check(
    r#"
type t = { 0: int, 1: bool }
(**        ^ invalid numeric label: numeric labels start at 1 *)
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

#[test]
fn t_05() {
  check(
    r#"
type t = { ~3: int, 1: bool }
(**        ^^ invalid numeric label: invalid digit found in string *)
"#,
  );
}

#[test]
fn t_06() {
  check(
    r#"
type t = { 0x3: int, 1: bool }
(**        ^^^ invalid numeric label: invalid digit found in string *)
"#,
  );
}
