//! Extra separators.

use crate::check::fail;

#[test]
fn exp_arg() {
  fail(
    r#"
val _ = (1,,2)
(**        ^ extra `,` *)
"#,
  );
}

#[test]
fn exp_row() {
  fail(
    r#"
val _ = {a = 1,, b = 2}
(**           ^ extra `,` *)
"#,
  );
}

#[test]
fn pat_arg() {
  fail(
    r#"
val _ = fn (x,,y) => ()
(**           ^ extra `,` *)
"#,
  );
}

#[test]
fn pat_row() {
  fail(
    r#"
val _ = fn {a = _,, b = _} => ()
(**               ^ extra `,` *)
"#,
  );
}

#[test]
fn ty_arg() {
  fail(
    r#"
type ('a, 'b) foo = unit
val _ : (int,, string) foo = ()
(**          ^ extra `,` *)
"#,
  );
}

#[test]
fn ty_row() {
  fail(
    r#"
val _ : {a: int,, b: int} = {a = 1, b = 2}
(**             ^ extra `,` *)
"#,
  );
}

#[test]
fn ty_var() {
  fail(
    r#"
type ('a,,'b) foo = unit
(**      ^ extra `,` *)
"#,
  );
}

#[test]
fn let_exp() {
  fail(
    r#"
val _ = let in 1;; 2 end
(**              ^ extra `;` *)
"#,
  );
}

#[test]
fn seq_exp() {
  fail(
    r#"
val _ = (1;; 2)
(**        ^ extra `;` *)
"#,
  );
}
