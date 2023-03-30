//! Miscellaneous tests. If unsure where to put a test, put it here.

use crate::check::check;

#[test]
fn arrow_ty_arg() {
  check(
    r#"
val _ = fn f => fn x =>
  (
    f x x;
    f x;
    f x x x andalso false;
    f 3;
(** ^ expected `int -> ?b`, found `unit` *)
    f: unit;
    false
  )
"#,
  );
}

#[test]
fn exhaustive_binding() {
  check(
    r#"
datatype t = C of int * bool
val C (a, b) = C (1, false)
val _ = if b then a + 2 else a - 4
"#,
  );
}

#[test]
fn fun_dec_name_mismatch() {
  check(
    r#"
fun f 1 = 1
  | g _ = 2
(** ^ expected a function clause for `f`, found one for `g` *)
"#,
  );
}

#[test]
fn fun_dec_wrong_num_pats() {
  check(
    r#"
fun f 1 = 2
  | f 3 4 = 5
(** ^^^^^^^^^ expected 1 pattern, found 2 *)
"#,
  );
}

#[test]
fn match_record_non_record_ty() {
  check(
    r#"
val _ =
  case 0 of
    1 => 1
  | (2, 2) => 2
(** ^^^^^^ expected `int`, found `int * int` *)
  | _ => 3
"#,
  );
}

#[test]
fn non_var_in_as() {
  check(
    r#"
exception Bad
val _ =
  case 3 of
    Bad as _ => 1
(** ^^^^^^^^ invalid `as` pat name: `Bad` *)
  | _ => 2
"#,
  );
}

#[test]
fn not_arrow_ty() {
  check(
    r#"
val _ = "foo" 3
(**     ^^^^^ expected `int -> ?b`, found `string` *)
"#,
  );
}

#[test]
fn ty_var_fixed() {
  check(
    r#"
val _ = fn id => (id 3; id "nope")
(**                  ^ expected `string`, found `int` *)
"#,
  );
}

#[test]
fn useless_ty_var() {
  check(
    r#"
fun 'a f () = 3
val _ = f: unit
(**     ^^^^^^^ expected `unit`, found `unit -> int` *)
"#,
  );
}

#[test]
fn value_restriction() {
  check(
    r#"
val id = (fn x => x) (fn x => x)
(**      ^^^^^^^^^^^^^^^^^^^^^^^ cannot bind expansive polymorphic expression *)
"#,
  );
}

#[test]
fn wrong_num_ty_args() {
  check(
    r#"
val _: (int, bool) list = []
(**    ^^^^^^^^^^^^^^^^ expected 1 type argument, found 2 *)
"#,
  );
}

#[test]
fn paren() {
  check(
    r#"
structure S = struct
  type i = (int)
  type bl = ((bool) list)
  val () = ()
  val a = 1
  val (b) = 2
  val c = (3)
  val (d) = (4)
  val (e, f) = (5, 6)
end
"#,
  );
}

#[test]
fn prec() {
  check(
    r#"
val _ = 1 = 2 andalso 3 > 4 orelse 5 < 6
"#,
  );
}

#[test]
fn phantom_datatype() {
  check(
    r#"
datatype 'a phantom = p
type t = string phantom
val _ = p: unit phantom
val _ = p: real phantom
"#,
  );
}

#[test]
fn phantom_type() {
  check(
    r#"
type 'a phantom = int
type t = string phantom
val _ = 3: unit phantom
val _ = 3: real phantom
"#,
  );
}

#[test]
fn subst_inside() {
  check(
    r#"
signature SIG = sig
  datatype 'a d = D of 'a
  val join : 'a d d -> 'a
end

structure Str :> SIG = struct
  datatype 'a d = D of 'a
  fun join (D (D x)) = x
end
"#,
  );
}

#[test]
fn semicolon() {
  check(
    r#"
val a = 3;
datatype foo = Bar;
fun inc x = let val y = x + 1; in "ignored"; y end;
val _ = inc a : int;
"#,
  );
}

#[test]
fn circular_datatype() {
  check(
    r#"
datatype foo = Foo of bar
and bar = Bar of foo | Quz
"#,
  );
}

#[test]
fn datatype_ty_vars_scope() {
  check(
    r#"
datatype 'a foo = Foo of 'a
and 'a bar = Bar of 'a
val _ = Bar (Foo 3) : int foo bar
"#,
  );
}

#[test]
fn ty_var_order_1() {
  check(
    r#"
type ('a, 'b, 'c) foo = 'b * 'a * 'c

structure NoSig = struct
  type ('a, 'b, 'c) foo = 'b * 'a * 'c
end

structure YesSig : sig
  type ('a, 'b, 'c) foo = 'b * 'a * 'c
end = struct
  type ('a, 'b, 'c) foo = 'b * 'a * 'c
end

val x = ("hi", 1, 1.1)
val _ = x : (int, string, real) foo
val _ = x : (int, string, real) NoSig.foo
val _ = x : (int, string, real) YesSig.foo
"#,
  );
}

#[test]
fn ty_var_order_2() {
  check(
    r#"
fun ('a, 'b) pair (x: 'a) (y: 'b) = (y, x)
val _ = pair 1 "hi" : string * int
"#,
  );
}

#[test]
fn str_dec_in_regular_dec() {
  check(
    r#"
val () =
  let
    structure S = struct end
(** ^^^^^^^^^^^^^^^^^^^^^^^^ structure-level declaration not allowed here *)
  in
    ()
  end
"#,
  );
}

#[test]
fn suggest_kw() {
  check(
    r#"
val lam = lambda x: x + 1
(**       ^^^^^^ undefined value: `lambda` (did you mean `fn`?) *)
"#,
  );
}

#[test]
fn op_bool_op() {
  check(
    r#"
val _ = op andalso
(**     ^^^^^^^^^^ `andalso` and `orelse` not allowed with `op` *)
"#,
  );
}

#[test]
fn top_exp() {
  check(
    r#"
fun foo () = ();
foo ();
foo ();
val x = foo ()
val y = 1 + 2;
foo ()
val z = 3 + 4
"#,
  );
}

#[test]
fn invalid_exp_expected_dec() {
  check(
    r#"
fun foo x =
  let
    val y = 3
    if x = 4 then y = 5 else ()
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^ expression not allowed here *)
  in
    y + x
  end
"#,
  );
}

#[test]
fn num_suffix() {
  check(
    r#"
val _ = 3and _ = 4
(**     ^^^^ invalid literal: invalid digit found in string *)
"#,
  );
}

#[test]
fn sharing_type_tail() {
  check(
    r#"
val _ = () sharing type a = b
(**        ^^^^^^^^^^^^^^^^^^ `sharing type` not allowed here *)
"#,
  );
}

#[test]
fn eqtype_dec() {
  check(
    r#"
    eqtype n = int
(** ^^^^^^ `eqtype` not allowed here *)
"#,
  );
}

// see https://github.com/rust-analyzer/rowan/issues/144, which we used to panic on but now don't
// because we just return None when we couldn't find the syntax node
#[test]
fn rowan_panic() {
  check(
    r#"
(** - expected `(` *)
functor MkThing"#,
  );
}

#[test]
fn op_plus_reduce() {
  check(
    r#"
datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
fun reduce (op +) z t =
  case t of
    Empty => z
  | Node (l, x, r) => reduce (op +) z l + x + reduce (op +) z r
"#,
  );
}

#[test]
fn op_false() {
  check(
    r#"
fun wot (op false) = ()
(** + missing true *)
"#,
  );
}

#[test]
fn infix_scope() {
  check(
    r#"
fun add (a, b) = a + b

val _ = add (1, 2)

structure S = struct
  infix add
  val _ = 3 add 4
  val _ = op add (5, 6)
end

val _ = add (7, 8)
"#,
  );
}
