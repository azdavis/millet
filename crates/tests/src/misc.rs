use crate::check::{check, fail};

#[test]
fn apply() {
  check(
    r#"
val apply = fn (f, x) => f x
val _ = apply: unit
(**     ^^^^^^^^^^^ expected unit, found (?a -> ?b) * ?a -> ?b *)
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
(** ^^^^^^^ expected unit, found int -> int -> int -> bool *)
  false
)
"#,
  );
}

#[test]
fn circularity_1() {
  check(
    r#"
    fun f _ = f
(** ^^^^^^^^^^^ attempted to a set a type variable ?b to a type containing that variable: ?a -> ?b *)
"#,
  );
}

#[test]
fn circularity_2() {
  check(
    r#"
fun f x = x x
(**       ^^^ attempted to a set a type variable ?a to a type containing that variable: ?a -> ?b *)
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
(** ^^^^^^^^^^^^^^^^^^^ expected unit, found ?a tree -> (?a -> bool) -> (?a -> ?b) -> (unit -> ?b) -> ?b *)
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
exception Empty
fun append [] ys = ys
  | append (x :: xs) ys = x :: append xs ys
and head [] = raise Empty
  | head (x :: _) = x
and tail [] = raise Empty
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
(**     ^^^^^^^^^ expected unit, found (?a -> ?b) -> ?a list -> ?b list *)
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
val _ = "foo" 3
(**     ^^^^^ expected a function type, found string *)
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
fn basic_std_lib_types() {
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
val _ = ref
and f = op+
val _ = f (a, 2)
and _: real = b / b
and _: int = 3 div 0
"#,
  );
}

#[test]
fn ty_var_fixed() {
  check(
    r#"
val _ = fn id =>
  (id 3; id "nope")
(**         ^^^^^^ expected int, found string *)
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
  val uh: 'a -> 'a thing
  type guy = { foo: int, bar: string }
  val hi: guy
end

structure Thing :> THING = struct
  type 'a t = 'a list
  type 'a thing = 'a t
  val uh = fn x => [x]
  type guy = { foo: int, bar: string }
  val hi = { foo = 4, bar = "yes" }
end
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
fn poly_exn_sig() {
  check(
    r#"
signature S = sig
  exception E of 'a
(**              ^^ undefined type variable: 'a *)
end
"#,
  );
}

#[test]
fn sharing_in_seq() {
  check(
    r#"
signature FOO = sig type t structure S: sig type t end end
signature BAR = sig structure Foo : FOO end
signature QUZ = sig structure Foo : FOO end

signature SIG = sig
  structure Foo : FOO
  structure Bar : BAR
  structure Quz : QUZ
  sharing Foo = Bar.Foo = Quz.Foo
  val x : Foo.S.t
end

functor F (
  structure Foo : FOO
  structure Bar : BAR
  structure Quz : QUZ
  sharing Foo = Bar.Foo = Quz.Foo
  val x : Foo.S.t
) = struct end
"#,
  );
}

#[test]
fn sig_ty_eq_undef() {
  check(
    r#"
signature S = sig
  type 'a t = 'a uh
(**           ^^^^^ undefined type: uh *)
end"#,
  );
}

#[test]
fn sig_ty_eq_wrong_num_ty_args() {
  check(
    r#"
signature S = sig
  datatype uh = Uh
  type 'a t = 'a uh
(**           ^^^^^ expected 0 type arguments, found 1 *)
end"#,
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
fn where_not_all_ty_vars() {
  check(
    r#"
signature SIG = sig
  type 'a t
  val f : 'a t -> 'a t
end

structure Str :> SIG where type 'a t = int = struct
  type 'a t = int
  fun f x = x
end

val _ = Str.f 3 : int
"#,
  );
}

#[test]
fn either() {
  check(
    r#"
datatype ('a, 'b) either = INL of 'a | INR of 'b
val _ = INL 3 : (int, unit) either
val _ = INR 3 : (unit, int) either
"#,
  );
}
