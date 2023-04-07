//! Tests for go to def.

use crate::check::check;

#[test]
fn val() {
  check(
    r#"
val x = 3
(** ^ def: x *)
val y = x
(**     ^ use: x *)
"#,
  );
}

#[test]
fn fun() {
  check(
    r#"
fun foo () = ()
(** ^^^^^^^^^^^ def: foo *)
val y = foo
(**     ^^^ use: foo *)
"#,
  );
}

#[test]
fn type_() {
  check(
    r#"
type bar = int
(** + def: bar *)
val y : bar = 3
(**     ^^^ use: bar *)
"#,
  );
}

#[test]
fn datatype_con() {
  check(
    r#"
datatype foo = Bar | Quz of int
(** + def: Bar *)
val y = Bar
(**     ^^^ use: Bar *)
"#,
  );
}

#[test]
fn structure() {
  check(
    r#"
structure A = struct end
(** + def: A *)
structure B = A
(**           ^ use: A *)
"#,
  );
}

#[test]
fn structure_open() {
  check(
    r#"
structure A1 = struct
  structure A2 = struct end
  (** + def: A2 *)
end

structure B1 = struct
  open A1
  structure B2 = A2
(**              ^^ use: A2 *)
end
"#,
  );
}
