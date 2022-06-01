use crate::check::fail;

#[test]
fn exp() {
  fail(
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
  fail(
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
  fail(
    r#"
fun f (_, _) = 1
fun _ g _ = 2
(**   ^ infix name used as non-infix without `op` *)
infix h
fun _ h _ = 3
"#,
  );
}
