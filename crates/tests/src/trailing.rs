//! Trailing `,` and `;`.

use crate::check::fail;

#[test]
fn exp_arg_1() {
  fail(
    r#"
val _ = (1,)
(**       ^ trailing comma *)
"#,
  );
}

#[test]
fn exp_arg_2() {
  fail(
    r#"
val _ = (1,2,)
(**         ^ trailing comma *)
"#,
  );
}

#[test]
fn exp_row_1() {
  fail(
    r#"
val _ = {a = 1,}
(**           ^ trailing comma *)
"#,
  );
}

#[test]
fn exp_row_2() {
  fail(
    r#"
val _ = {a = 1, b = 2,}
(**                  ^ trailing comma *)
"#,
  );
}

#[test]
fn pat_arg_1() {
  fail(
    r#"
val _ = fn (x,) => ()
(**          ^ trailing comma *)
"#,
  );
}

#[test]
fn pat_arg_2() {
  fail(
    r#"
val _ = fn (x,y,) => ()
(**            ^ trailing comma *)
"#,
  );
}

#[test]
fn pat_row_1() {
  fail(
    r#"
val _ = fn {a = _,} => ()
(**              ^ trailing comma *)
"#,
  );
}

#[test]
fn pat_row_2() {
  fail(
    r#"
val _ = fn {a = _, b = _,} => ()
(**                     ^ trailing comma *)
"#,
  );
}

#[test]
fn ty_arg_1() {
  fail(
    r#"
type 'a foo = unit
val _ : (int,) foo = ()
(**         ^ trailing comma *)
"#,
  );
}

#[test]
fn ty_arg_2() {
  fail(
    r#"
type ('a, 'b) foo = unit
val _ : (int,string,) foo = ()
(**                ^ trailing comma *)
"#,
  );
}

#[test]
fn ty_row_1() {
  fail(
    r#"
val _ : {a: int,} = {a = 1}
(**            ^ trailing comma *)
"#,
  );
}

#[test]
fn ty_row_2() {
  fail(
    r#"
val _ : {a: int, b: int,} = {a = 1, b = 2}
(**                    ^ trailing comma *)
"#,
  );
}

#[test]
fn ty_var_1() {
  fail(
    r#"
type ('a,) foo = unit
(**     ^ trailing comma *)
"#,
  );
}

#[test]
fn ty_var_2() {
  fail(
    r#"
type ('a, 'b,) foo = unit
(**         ^ trailing comma *)
"#,
  );
}

#[test]
fn exp_in_seq_1() {
  fail(
    r#"
val _ = let in 1; end
(**             ^ trailing semicolon *)
"#,
  );
}

#[test]
fn exp_in_seq_2() {
  fail(
    r#"
val _ = let in 1; 2; end
(**                ^ trailing semicolon *)
"#,
  );
}
