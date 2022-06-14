use crate::check::check;

#[test]
fn smoke() {
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
fn exn() {
  check(
    r#"
datatype ok = datatype exn
"#,
  );
}

#[test]
fn int() {
  check(
    r#"
datatype yes = datatype int
"#,
  );
}

#[test]
fn with_ty_var() {
  check(
    r#"
datatype vec = datatype list
val _: int vec = [1, 2]
"#,
  );
}

#[test]
fn structure() {
  check(
    r#"
structure S = struct datatype a = A end
datatype b = datatype S.a
val _ = A: b
val _ = A: S.a
val _ = S.A: b
val _ = S.A: S.a
  "#,
  );
}
