use crate::check::{check, fail};

#[test]
fn val() {
  check(
    r#"
val x = 3
val _: int = x
"#,
  );
}

#[test]
fn fun() {
  check(
    r#"
fun three () = 3
val _: unit -> int = three
val _: int = three ()
"#,
  );
}

#[test]
fn typ() {
  check(
    r#"
type a = int
val _: a = 3
"#,
  );
}

#[test]
fn datatype() {
  check(
    r#"
datatype d = A | B of int
val _: d = A
val _: int -> d = B
val _: d = B 3
"#,
  );
}

#[test]
fn datatype_copy() {
  check(
    r#"
datatype one = A
datatype two = datatype one
"#,
  );
}

#[test]
fn exception() {
  check(
    r#"
exception A
val _: exn = A
val _ = fn () => raise A
exception B of int
val _: int -> exn = B
val _ = fn () => raise B 3
exception C = A
val _: exn = C
val _ = fn () => raise C
"#,
  );
}

#[test]
fn local() {
  check(
    r#"
val _ =
  let
    local
      val x = 3
    in
      val y = x
    end
  in
    y
  end
"#,
  );
}

#[test]
fn open() {
  fail(
    r#"
structure S = struct
  val x = 3
end
open S
val _: int = x
"#,
  );
}

#[test]
fn fixity() {
  check(
    r#"
infix hi
fun (() hi ()) = 3
val _: int = () hi ()

infixr uh
fun (() uh ()) = 3
val _: int = () uh ()

nonfix hi uh
val _: int = hi ((), ())
val _: int = uh ((), ())
"#,
  );
}
