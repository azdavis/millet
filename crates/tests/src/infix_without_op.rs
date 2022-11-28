//! Using infix operators without `op`.

use crate::check::check;

#[test]
fn exp() {
  check(
    r#"
datatype d = D of int * string
val D _ = D (3, "hi")
infix D
val _ D _ = D (3, "hi")
(**         ^ infix name used as non-infix without `op` *)
"#,
  );
}

#[test]
fn pat() {
  check(
    r#"
datatype d = D of int * string
val D _ = D (3, "hi")
infix D
val D _ = 3 D "hi"
(** ^ infix name used as non-infix without `op` *)
"#,
  );
}

#[test]
fn fun_head() {
  // with some effort, this could be the actual infix without op error
  check(
    r#"
fun f (_, _) = 1
fun _ g _ = 2
(** ^ expected a name *)
infix h
fun _ h _ = 3
"#,
  );
}

#[test]
fn cons_not_atomic() {
  check(
    r#"
fun map f [] = []
  | map f x::xs = f x :: map f xs
(**        ^^ infix name used as non-infix without `op` *)
"#,
  );
}
