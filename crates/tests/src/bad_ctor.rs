use crate::check::check;

#[test]
fn t_01() {
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
fn t_02() {
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
