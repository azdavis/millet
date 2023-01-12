//! Extra separators.

use crate::check::check;

#[test]
fn exp_arg() {
  check(
    r#"
val _ = (1,,2)
(**        ^ expected an expression *)
"#,
  );
}

#[test]
fn exp_row() {
  check(
    r#"
val _ = {a = 1,, b = 2}
(**            ^ expected a label *)
"#,
  );
}

#[test]
fn pat_arg() {
  check(
    r#"
val _ = fn (x,,y) => ()
(**           ^ expected a pattern *)
"#,
  );
}

#[test]
fn pat_row() {
  check(
    r#"
val _ = fn {a = _,, b = _} => ()
(**               ^ expected a name *)
"#,
  );
}

#[test]
fn ty_arg() {
  check(
    r#"
type ('a, 'b) foo = unit
val _ : (int,, string) foo = ()
(**          ^ expected a type *)
"#,
  );
}

#[test]
fn ty_row() {
  check(
    r#"
val _ : {a: int,, b: int} = {a = 1, b = 2}
(**             ^ expected a label *)
"#,
  );
}

#[test]
fn ty_var() {
  check(
    r#"
type ('a,,'b) foo = unit
(**      ^ expected a type variable *)
"#,
  );
}

#[test]
fn let_exp() {
  check(
    r#"
val _ = let in 1;; 2 end
(**              ^ expected an expression *)
"#,
  );
}

#[test]
fn seq_exp() {
  check(
    r#"
val _ = (1;; 2)
(**        ^ expected an expression *)
"#,
  );
}
