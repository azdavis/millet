//! Tests for completions.

use crate::check::check;

#[test]
fn smoke() {
  check(
    r#"
val foo = 3

(**      vvv expected an expression *)
val _ =  val
(**     ^ completions(with-std): foo *)
"#,
  );
}

#[test]
fn in_struct() {
  check(
    r#"
structure Foo = struct
  val bar = 3
  val quz = "hi"
end

(**          vvv expected a name *)
val _ = Foo. val
(**         ^ completions: bar, quz *)
"#,
  );
}

#[test]
fn nested() {
  check(
    r#"
structure A = struct
  val x = 3
  structure B = struct
    val y = 4
  end
end

(**          vvv expected a name *)
val _ = A.B. val
(**         ^ completions: y *)
"#,
  );
}
