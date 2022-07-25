//! We parse (but reject in later stages of analysis) some [SuccessorML][1] features.
//!
//! [1]: http://mlton.org/SuccessorML

use crate::check::check;

#[test]
fn do_dec() {
  check(
    r#"
    fun print _ = ()
    do print "hi"
(** ^^^^^^^^^^^^^ unsupported language construct: `do` declarations *)
"#,
  );
}

#[test]
fn preceding_bar_fun() {
  check(
    r#"
    fun
    | f 1 = 2
(** ^ preceding `|` *)
    | f 3 = 4
    | f _ = 5
"#,
  );
}

#[test]
fn preceding_bar_fn() {
  check(
    r#"
    val f = fn
    | 1 => 2
(** ^ preceding `|` *)
    | 3 => 4
    | _ => 5
"#,
  );
}

#[test]
fn preceding_bar_case() {
  check(
    r#"
  fun f x =
    case x of
    | 1 => 2
(** ^ preceding `|` *)
    | 3 => 4
    | _ => 5
"#,
  );
}

#[test]
fn preceding_bar_handle() {
  check(
    r#"
  exception A and B
  val _ = 1 handle
    | A => 2
(** ^ preceding `|` *)
    | B => 3
"#,
  );
}

#[test]
fn preceding_bar_datatype() {
  check(
    r#"
    datatype t =
    | A
(** ^ preceding `|` *)
    | B
    | C
"#,
  );
}

#[test]
fn exp_row_pun() {
  check(
    r#"
fun incB r =
  case r of {a, b, c} => {a, b = b + 1, c}
(**                       ^ unsupported language construct: expression row punning *)
"#,
  )
}
