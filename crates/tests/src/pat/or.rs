//! note that we do not require () around the or pattern alternatives, while SML/NJ appears to.

use crate::check::check;

#[test]
fn smoke() {
  check(
    r#"
val _ =
  case 123 of
    1 | 2 => false
  | _ => true
"#,
  );
}

#[test]
fn not_all_same_name() {
  check(
    r#"
datatype t = A of int | B of int
fun f (A x | B y) = x
(**          ^^^ x was bound in one alternative, but not in another *)
"#,
  );
}

#[test]
fn not_all_same_ty() {
  check(
    r#"
datatype t = A of int | B of string
fun f (A x | B x) = x
(**          ^^^ expected ?a, found int *)
"#,
  );
}

#[test]
fn exhaustive() {
  check(
    r#"
datatype t = A of int | B of int
fun f (A x | B x) = x
"#,
  );
}

#[test]
fn in_ctor() {
  check(
    r#"
datatype t = A of int | B of int
fun f x =
  case x of
    A (1 | 2) => 1
  | A (3 | 4) | B (5 | 6) => 2
  | B (7 | 8 | 9 | 10) => 3
  | _ => 4
"#,
  );
}

#[test]
fn unreachable_smoke() {
  check(
    r#"
fun f x =
  case x of
    1 | 1 => 1
(**     ^ unreachable pattern *)
  | _ => 2
"#,
  );
}

#[test]
fn unreachable_complex() {
  check(
    r#"
datatype t = A of int | B of int
fun f x =
  case x of
    A (1 | 2) => 1
  | B 3 | A 2 => 2
(**         ^ unreachable pattern *)
  | _ => 2
"#,
  );
}
