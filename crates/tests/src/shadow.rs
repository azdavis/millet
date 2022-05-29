use crate::check::check;

#[test]
fn t_01() {
  check(
    r#"
val x = "hey"
val x = 3
val _: int = x
"#,
  );
}

#[test]
fn t_02() {
  check(
    r#"
type t = string
type t = int
val _: t = 3
"#,
  );
}

#[test]
fn t_03() {
  check(
    r#"
datatype t = One
datatype t = Two
val _: t = Two
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn t_04() {
  check(
    r#"
structure S = struct val x = "hey" end
structure S = struct val x = 3 end
val _: int = S.x
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn t_05() {
  check(
    r#"
exception E
exception E of int
    val _: unit = E
(** ^^^^^^^^^^^^^^^ mismatched types: expected unit, found int -> exn *)
"#,
  );
}
