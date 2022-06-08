use crate::check::check;

#[test]
fn val() {
  check(
    r#"
val x = "hey"
val x = 3
val _: int = x
"#,
  );
}

#[test]
fn type_() {
  check(
    r#"
type t = string
type t = int
val _: t = 3
"#,
  );
}

#[test]
fn datatype() {
  check(
    r#"
datatype t = One
datatype t = Two
val _: t = Two
"#,
  );
}

#[test]
fn structure() {
  check(
    r#"
structure S = struct val x = "hey" end
structure S = struct val x = 3 end
val _: int = S.x
"#,
  );
}

#[test]
fn exception() {
  check(
    r#"
exception E
exception E of int
val _ = E: unit
(**     ^^^^^^^ expected unit, found int -> exn *)
"#,
  );
}
