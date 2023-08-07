//! We parse (but reject in later stages of analysis) some [Successor ML][1] features.
//!
//! [1]: http://mlton.org/SuccessorML

use crate::check::{check, check_multi, raw};

#[test]
fn do_dec_default_disallow() {
  check(
    r#"
    fun print _ = ()
    do print "hi"
(** ^^^^^^^^^^^^^ disallowed *)
"#,
  );
}

#[test]
fn do_dec_config_allow() {
  let config = r#"
version = 1
language.successor-ml.do-dec = true
"#;
  let sml = r#"
fun print _ = ()
do print "hi"

val x =
  let
    val y = 4
    do print "hi... "
    val z = y + 2
    do print "bye"
  in
    y * z * z
  end

do ()
"#;
  check_multi(raw::singleton(config, sml));
}

#[test]
fn do_dec_exp_must_be_unit() {
  let config = r#"
version = 1
language.successor-ml.do-dec = true
"#;
  let sml = r#"
do "hi"
(** + expected `unit`, found `string` *)
"#;
  check_multi(raw::singleton(config, sml));
}

#[test]
fn preceding_bar_case_disallow() {
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
fn preceding_bar_fn_disallow() {
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
fn preceding_bar_handle_disallow() {
  check(
    r#"
  exception A and B of int
  val _ = 1 handle
    | A => 2
(** ^ preceding `|` *)
    | B _ => 3
"#,
  );
}

#[test]
fn preceding_bar_fun_disallow() {
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
fn preceding_bar_datatype_disallow() {
  check(
    r#"
    datatype t =
    | A
(** ^ preceding `|` *)
    | B of int
    | C
    | D of string
"#,
  );
}

#[test]
fn exp_row_pun_disallow() {
  check(
    r#"
fun incB r =
  case r of {a, b, c} => {a, b = b + 1, c}
(**                       ^ unsupported: expression row punning *)
"#,
  );
}

#[test]
fn withtype_sig_disallow() {
  check(
    r#"
signature STREAM =
  sig
    datatype 'a u = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a u
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ unsupported: `withtype` in specifications *)
  end
"#,
  );
}

#[test]
fn or_pat_default_allow() {
  check(
    r#"
datatype d = A of int | B of int
fun f (A x | B x) = x
"#,
  );
}

#[test]
fn or_pat_config_allow() {
  let config = r#"
version = 1
language.successor-ml.or-pat = true
"#;
  let sml = r#"
datatype d = A of int | B of int
fun f (A x | B x) = x
"#;
  check_multi(raw::singleton(config, sml));
}

#[test]
fn or_pat_config_disallow() {
  let config = r#"
version = 1
language.successor-ml.or-pat = false
"#;
  let sml = r#"
datatype d = A of int | B of int
fun f (A x | B x) = x
(**    ^^^^^^^^^ disallowed Successor ML feature: or patterns *)
"#;
  check_multi(raw::singleton(config, sml));
}
