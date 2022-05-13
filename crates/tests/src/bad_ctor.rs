use crate::check::check;

#[test]
fn t_01() {
  check(
    r#"
datatype d = A | B of int
val _ =
  case A of
    A _ => 1
(** ^^^ mismatched types: expected an arrow type, found d *)
  | B _ => 2
"#,
  );
}

#[test]
fn t_02() {
  check(
    r#"
datatype d = A | B of int
val _ =
  case A of
    A => 1
  | B => 2
(** ^ mismatched types: expected a constructor type, found int -> d *)
"#,
  );
}
