//! Equality types.

use crate::check::{check, fail};

#[test]
fn real_no() {
  check(
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
  check(
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
fn datatype_sometimes_1() {
  check(
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
fn datatype_sometimes_2() {
  check(
    r#"
datatype 'a t = A of 'a | B of int | C of string | D
fun ''a eq (x : ''a t) (y : ''a t) : bool = x = y
fun 'a eq (x : 'a t) (y : 'a t) : bool = x = y
(** + contains: contains a fixed non-equality type variable *)
"#,
  );
}

#[test]
fn datatype_eq_ty_var_1() {
  check(
    r#"
datatype ''a t = A of ''a | B
fun ''a eq (x : ''a t) (y : ''a t) : bool = x = y
fun 'a yes (x : 'a t) : unit = ()
"#,
  );
}

#[test]
fn datatype_eq_ty_var_2() {
  check(
    r#"
datatype ''a t = A of ''a | B
fun 'a no (x : 'a) : 'a t = A x
(** + contains: not an equality type *)
"#,
  );
}

#[test]
fn datatype_eq_ty_var_3() {
  check(
    r#"
datatype ''a t = A of ''a | B
fun 'a no (x : 'a) : 'a t = B
(** + contains: not an equality type *)
"#,
  );
}

#[test]
fn datatype_eq_ty_var_4() {
  check(
    r#"
datatype ''a t = A of ''a | B
exception E
fun 'a yes (x : 'a) : 'a t = raise E
"#,
  );
}

#[test]
fn datatype_no_real() {
  fail(
    r#"
datatype t = B of real | C of string | D
val _ = C "hi" = D
(**     ^^^^^^^^^^ contains: not an equality type *)
"#,
  );
}
