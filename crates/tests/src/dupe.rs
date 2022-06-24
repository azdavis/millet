use crate::check::check;

mod spec;

#[test]
fn datatype() {
  check(
    r#"
    datatype t = A and t = B
(** ^^^^^^^^^^^^^^^^^^^^^^^^ duplicate type: t *)
"#,
  );
}

#[test]
fn exn() {
  check(
    r#"
    exception E and E
(** ^^^^^^^^^^^^^^^^^ duplicate value: E *)
"#,
  );
}

#[test]
fn ty_var_datatype() {
  check(
    r#"
    datatype ('a, 'a) t = A of 'a
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ duplicate type variable: 'a *)
"#,
  );
}

#[test]
fn ty_var_fun() {
  check(
    r#"
    fun ('a, 'a) f (x: 'a) = 3
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^ duplicate type variable: 'a *)
"#,
  );
}

#[test]
fn ty_var_type() {
  check(
    r#"
    type ('a, 'a) foo = int
(** ^^^^^^^^^^^^^^^^^^^^^^^ duplicate type variable: 'a *)
"#,
  );
}

#[test]
fn ty_var_val() {
  check(
    r#"
    val ('a, 'a) _: 'a list = []
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ duplicate type variable: 'a *)
"#,
  );
}

#[test]
fn type_() {
  check(
    r#"
    type t = int and t = string
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^ duplicate type: t *)
"#,
  );
}

#[test]
fn val() {
  check(
    r#"
val x = 3 and x = 4
(**           ^ duplicate value: x *)
"#,
  );
}

#[test]
fn var_fn() {
  check(
    r#"
val _ = fn (x, x) => 3
(**            ^ duplicate value: x *)
"#,
  );
}

#[test]
fn var_fun() {
  check(
    r#"
fun f (x, x) = 3
(**       ^ duplicate value: x *)
"#,
  );
}

#[test]
fn var_val() {
  check(
    r#"
val (x, x) = (1, 2)
(**     ^ duplicate value: x *)
"#,
  );
}

#[test]
fn label() {
  check(
    r#"
val _ = { a = 3, a = 4 }
(**     ^^^^^^^^^^^^^^^^ duplicate label: a *)
"#,
  );
}
