//! Trailing separators.

use crate::check::check;

#[test]
fn exp_arg_1() {
  check(
    r#"
val _ = (1,)
(**       ^ trailing `,` *)
"#,
  );
}

#[test]
fn exp_arg_2() {
  check(
    r#"
val _ = (1,2,)
(**         ^ trailing `,` *)
"#,
  );
}

#[test]
fn exp_row_1() {
  check(
    r#"
val _ = {a = 1,}
(**           ^ trailing `,` *)
"#,
  );
}

#[test]
fn exp_row_2() {
  check(
    r#"
val _ = {a = 1, b = 2,}
(**                  ^ trailing `,` *)
"#,
  );
}

#[test]
fn pat_arg_1() {
  check(
    r#"
val _ = fn (x,) => ()
(**          ^ trailing `,` *)
"#,
  );
}

#[test]
fn pat_arg_2() {
  check(
    r#"
val _ = fn (x,y,) => ()
(**            ^ trailing `,` *)
"#,
  );
}

#[test]
fn pat_row_1() {
  check(
    r#"
val _ = fn {a = _,} => ()
(**              ^ trailing `,` *)
"#,
  );
}

#[test]
fn pat_row_2() {
  check(
    r#"
val _ = fn {a = _, b = _,} => ()
(**                     ^ trailing `,` *)
"#,
  );
}

#[test]
fn ty_arg_1() {
  check(
    r#"
type 'a foo = unit
val _ : (int,) foo = ()
(**         ^ trailing `,` *)
"#,
  );
}

#[test]
fn ty_arg_2() {
  check(
    r#"
type ('a, 'b) foo = unit
val _ : (int,string,) foo = ()
(**                ^ trailing `,` *)
"#,
  );
}

#[test]
fn ty_row_1() {
  check(
    r#"
val _ : {a: int,} = {a = 1}
(**            ^ trailing `,` *)
"#,
  );
}

#[test]
fn ty_row_2() {
  check(
    r#"
val _ : {a: int, b: int,} = {a = 1, b = 2}
(**                    ^ trailing `,` *)
"#,
  );
}

#[test]
fn ty_var_1() {
  check(
    r#"
type ('a,) foo = unit
(**     ^ trailing `,` *)
"#,
  );
}

#[test]
fn ty_var_2() {
  check(
    r#"
type ('a, 'b,) foo = unit
(**         ^ trailing `,` *)
"#,
  );
}

#[test]
fn exp_in_seq_1() {
  check(
    r#"
val _ = let in 1; end
(**             ^ trailing `;` *)
"#,
  );
}

#[test]
fn exp_in_seq_2() {
  check(
    r#"
val _ = let in 1; 2; end
(**                ^ trailing `;` *)
"#,
  );
}
