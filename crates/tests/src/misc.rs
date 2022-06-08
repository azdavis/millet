use crate::check::{check, fail};

#[test]
fn apply() {
  check(
    r#"
val apply = fn (f, x) => f x
val _ = apply: unit
(**     ^^^^^^^^^^^ expected unit, found (_ -> _) * _ -> _ *)
"#,
  );
}

#[test]
fn arrow_ty_arg() {
  check(
    r#"
val _ = fn f => fn x => (
  f x x;
  f x;
  f x x x andalso false;
  f 3;
    f: unit;
(** ^^^^^^^ expected unit, found _ *)
  false
)
"#,
  );
}

#[test]
fn circularity() {
  // TODO improve this error message
  check(
    r#"
    fun f _ = f
(** ^^^^^^^^^^^ circularity: _ -> _ *)
"#,
  );
}

#[test]
fn cps() {
  check(
    r#"
datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
fun find t p ok err =
  case t of
    Empty => err ()
  | Node (left, x, right) =>
      if p x then
        ok x
      else
        find left p ok (fn () => find right p ok err)
    val _ : unit = find
(** ^^^^^^^^^^^^^^^^^^^ expected unit, found _ tree -> (_ -> bool) -> (_ -> _) -> (unit -> _) -> _ *)
"#,
  );
}

#[test]
fn equality() {
  check(
    r#"
val _ = 2 = 3
val _ = false = true
val _ = 0w0 = 0w1
val _ = "foo" = "bar"
val _ = #"a" = #"b"
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
fn fact() {
  check(
    r#"
fun fact (0 : int) : int = 1
  | fact n = n * fact (n - 1)
"#,
  );
}

#[test]
fn forbidden_binding() {
  check(
    r#"
    datatype no = ref
(** ^^^^^^^^^^^^^^^^^ cannot re-bind name: ref *)
"#,
  );
}

#[test]
fn list_fns() {
  check(
    r#"
fun append [] ys = ys
  | append (x :: xs) ys = x :: append xs ys
and head [] = raise Match
  | head (x :: _) = x
and tail [] = raise Bind
  | tail [x] = x
  | tail (_ :: xs) = tail xs
val x = head [1, 2, 3]
and y = tail [false, true, false]
and z = append [1, 2, 3] [7, 8, 9]
"#,
  );
}

#[test]
fn fun_dec_name_mismatch() {
  check(
    r#"
fun f 1 = 1
  | g _ = 2
(** ^ expected a function clause for f, found one for g *)
"#,
  );
}

#[test]
fn fun_dec_wrong_num_pats() {
  check(
    r#"
fun f 1 = 2
  | f 3 4 = 5
(** ^^^^^^^^^ expected 1 patterns, found 2 *)
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
(** ^^^^^^ expected int, found int * int *)
  | _ => 3
"#,
  );
}

#[test]
fn id() {
  check(
    r#"
val id = fn x => x
val _ = id 3
val _ = id "hey"
val _ = if id false then id 1 + 1 else id (2 + 2)
"#,
  );
}

#[test]
fn implicit_ty_var_scope() {
  fail(
    r#"
fun id (x: 'a): 'a = x
val _ = id 3
val _ = id "hey"
"#,
  );
}

#[test]
fn inc() {
  check(
    r#"
val inc = fn x => x + 1
val _ = inc 3
val _ = inc "nope"
(**         ^^^^^^ expected int, found string *)
"#,
  );
}

#[test]
fn list_map() {
  check(
    r#"
fun map f xs =
  case xs of
    [] => []
  | x :: xs => f x :: map f xs

val _ = map: unit
(**     ^^^^^^^^^ expected unit, found (_ -> _) -> _ list -> _ list *)
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
(** ^^^^^^^^ invalid `as` pat name: Bad *)
  | _ => 2
"#,
  );
}

#[test]
fn not_arrow_ty() {
  check(
    r#"
val _ = 3 3
(**     ^ expected a function type, got int *)
"#,
  );
}

#[test]
fn not_equality() {
  fail(
    r#"
val _ = 2.2 = 3.3
(**     ^^^^^^^^^ not an equality type: real *)
"#,
  );
}

#[test]
fn std_lib_types() {
  check(
    r#"
val _: int = 3 + 3
and _: real = 3.3 + 3.3
(* what's this? *)
and _: word = 0w0 + 0w0
and _: bool list = [3 > 3]
and _ = "foo" > "bar"
val (a, b: real) = (123, 2.34)
val _ = #"e" > (#"f": char)
val _ = 3 = 4
val _ = if 123 < 234 then [LESS] else [GREATER, EQUAL]
val _ = ref
and f = op+
val _ = f (a, 2)
and _: real = b / b
and _: int = 3 div 0
"#,
  );
}

#[test]
fn ty_var_scope() {
  check(
    r#"
val _ = fn id =>
  (id 3; id "nope")
(**      ^^^^^^^^^ expected _, found string -> _ *)
"#,
  );
}

#[test]
fn useless_ty_var() {
  check(
    r#"
fun 'a f () = 3
val _ = f: unit
(**     ^^^^^^^ expected unit, found unit -> int *)
"#,
  );
}

#[test]
fn value_restriction() {
  fail(
    r#"
    val id = (fn x => x) (fn x => x)
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ oh no! *)
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
fn curry() {
  check(
    r#"
fun curry f x y = f (x, y)

val eq = curry op=
val add = curry op+
val mul = curry op*

val _: bool = eq 1 2
val _: int = add 3 4
val _: int = mul 5 6

val f = add 7
val g = mul 5
val _: bool = eq (f 3) (g 5)
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
fn symbol_ident_sig() {
  check(
    r#"
signature INT = sig
  val ~ : int -> int
  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  val div : int * int -> int
  val mod : int * int -> int
  val quot : int * int -> int
  val rem : int * int -> int
  val >  : int * int -> bool
  val >= : int * int -> bool
  val <  : int * int -> bool
  val <= : int * int -> bool
end
"#,
  );
}

#[test]
fn type_alias_in_sig() {
  check(
    r#"
signature THING = sig
  type 'a t
  type 'a thing = 'a t
  type guy = { foo: int, bar: string }
  val hi: guy
end
"#,
  );
}

#[test]
fn selector() {
  check(
    r#"
val _: int = #1 (3, "hi")
(**          ^^ unsupported language construct: `...` pattern rows *)
"#,
  );
}

#[test]
fn vector() {
  check(
    r#"
val _ = #[1, 2]
(**     ^^^^^^^ unsupported language construct: vector expressions *)
"#,
  );
}
