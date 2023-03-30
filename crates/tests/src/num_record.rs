//! Tests that witness the fact that tuples, records, and `unit` are all eventually records.

use crate::check::check;

#[test]
fn record_is_tuple() {
  check(
    r#"
type t = { 1: int, 2: bool }
val _ = (): t
(**     ^^^^^ expected `int * bool`, found `unit` *)
"#,
  );
}

#[test]
fn zero_not_label() {
  check(
    r#"
type t = { 0: int, 1: bool }
(**        ^ invalid numeric label: numeric labels start at 1 *)
"#,
  );
}

#[test]
fn not_tuple_if_only_one_label() {
  check(
    r#"
type t = { 1: int }
val _ = (): t
(**     ^^^^^ expected `{ 1 : int }`, found `unit` *)
"#,
  );
}

#[test]
fn not_tuple_if_not_all_labels_in_range() {
  check(
    r#"
type t = { 1: int, 3: bool }
val _ = (): t
(**     ^^^^^ expected `{ 1 : int, 3 : bool }`, found `unit` *)
"#,
  );
}

#[test]
fn neg_label() {
  check(
    r#"
type t = { ~3: int, 1: bool }
(**        ^^ invalid numeric label: invalid digit found in string *)
"#,
  );
}

#[test]
fn hex_label() {
  check(
    r#"
type t = { 0x3: int, 1: bool }
(**        ^^^ invalid numeric label: invalid digit found in string *)
"#,
  );
}
