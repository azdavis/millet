use crate::check::{check, fail};

#[test]
fn real_no() {
  fail(
    r#"
val _ = 2.2 = 3.3
(**     ^^^^^^^^^ contains: not an equality type *)
"#,
  );
}

#[test]
fn ref_yes() {
  check(
    r#"
val _ = (ref 2.2) = (ref 3.3)
"#,
  );
}

#[test]
fn sig_type_no() {
  fail(
    r#"
structure S = struct
  type t = int
  val x = 3
  val y = 4
end :> sig
  type t
  val x : t
  val y : t
end

val _ = S.x = S.y
(**     ^^^^^^^^^ contains: not an equality type *)
"#,
  );
}

#[test]
fn sig_eqtype_yes() {
  check(
    r#"
structure S = struct
  type t = int
  val x = 3
  val y = 4
end :> sig
  eqtype t
  val x : t
  val y : t
end

val _ = S.x = S.y
"#,
  );
}

#[test]
fn datatype_sometimes() {
  fail(
    r#"
datatype 'a t = A of 'a | B of int | C of string | D

val _ = B 3 = C "hi"
val _ = A () = D
val _ = A 3.3 = D
(**     ^^^^^^^^^ contains: not an equality type *)
"#,
  );
}

#[test]
fn datatype_no_ty_var() {
  fail(
    r#"
datatype ''a t = A of ''a | B of int | C of string | D

val _ = B 3 = C "hi"
(**     ^^^^^^^^^^^^ contains: not an equality type *)
"#,
  );
}

#[test]
fn datatype_no_real() {
  fail(
    r#"
datatype 'a t = A of 'a | B of real | C of string | D

val _ = C "hi" = D
(**     ^^^^^^^^^^ contains: not an equality type *)
"#,
  );
}
