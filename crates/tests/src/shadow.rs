//! Shadowing, as in `let a = false; let a = 2; let b = a + 3`.

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
(**     ^^^^^^^ contains: expected unit, found int -> exn *)
"#,
  );
}

#[test]
fn signature() {
  check(
    r#"
signature SIG = sig end
signature SIG = sig type t end

structure S: SIG = struct type t = int end
val _ = 3 : S.t
"#,
  );
}

#[test]
fn functor() {
  check(
    r#"
functor F() = struct end
functor F() = struct type t = int end

structure S = F()
val _ = 3 : S.t
"#,
  );
}
