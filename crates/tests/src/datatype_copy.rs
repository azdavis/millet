use crate::check::check;

#[test]
fn t_01() {
  check(
    r#"
datatype guh = A | B
datatype heh = datatype guh
fun f (x: heh): int =
  case x of
    A => 1
  | B => 2
val _ = f A + f B
"#,
  );
}

#[test]
fn t_02() {
  check(
    r#"
datatype bad = datatype exn
"#,
  );
}

#[test]
fn t_03() {
  check(
    r#"
datatype vec = datatype list
val _: int vec = [1, 2]
"#,
  );
}

#[test]
fn t_04() {
  check(
    r#"
datatype no = datatype int
"#,
  );
}
