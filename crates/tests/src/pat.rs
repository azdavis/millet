use crate::check::check;

#[test]
fn unexpected_arg_1() {
  check(
    r#"
val _ = fn nil _ => 1 | _ => 2
(**        ^^^^^ unexpected argument for constructor pattern *)
"#,
  );
}

#[test]
fn unexpected_arg_2() {
  check(
    r#"
datatype d = A | B of int
val _ =
  case A of
    A _ => 1
(** ^^^ unexpected argument for constructor pattern *)
  | B _ => 2
"#,
  );
}

#[test]
fn missing_arg_1() {
  check(
    r#"
val _ = fn op:: => 3
(**        ^^^^ missing argument for constructor pattern *)
"#,
  );
}

#[test]
fn missing_arg_2() {
  check(
    r#"
datatype d = A | B of int
val _ =
  case A of
    A => 1
  | B => 2
(** ^ missing argument for constructor pattern *)
"#,
  );
}

#[test]
fn wrong_id_status() {
  check(
    r#"
val C = 3
val _ =
  case 3 of
    C _ => 1
(** ^ mismatched identifier status: expected constructor or exception, found value *)
  | _ => 2
"#,
  );
}

#[test]
fn real() {
  check(
    r#"
val _ =
  case 123.123 of
    1.2 => 1
(** ^^^ real literal used as a pattern *)
  | _ => 2
"#,
  );
}

#[test]
fn infix() {
  check(
    r#"
datatype d = A | B of int * int
infix B
val f = fn
  A => 1
| x B y => x + y
nonfix B
val g = fn
  A => 2
| B tup => op- tup
val _: int = f A + g (B (1, 2))
infix B
val _: int = g A + f (2 B 3)
"#,
  );
}
