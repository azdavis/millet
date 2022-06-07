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
fn bind() {
  check(
    r#"
datatype bin = Zero | One
val One = One
(** ^^^ non-exhaustive binding: missing Zero *)
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
fn empty() {
  check("");
}

#[test]
fn no_top_items() {
  check(
    r#"
    and
(** ^^^ expected a top-level item *)
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
fn expected() {
  check(
    r#"
val _ 3
(**   ^ expected `=` *)
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
fn handle_non_exhaustive() {
  check(
    r#"
val _ = 3 handle Match => 1
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
fn negative_word_lit() {
  check(
    r#"
val _ = ~0w1
(**     ^^^^ negative word literal *)
"#,
  );
}

#[test]
fn non_exhaustive_binding() {
  check(
    r#"
val 3 = 1 + 2
(** ^ non-exhaustive binding: missing _ *)
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
fn not_infix() {
  check(
    r#"
datatype t = C of int * int
fun f (_ C _) = 2
(**      ^ non-infix name used as infix *)
"#,
  );
}

#[test]
fn option() {
  check(
    r#"
(* Rust's Option<T> *)

datatype 't option = None | Some of 't
exception Unwrap

fun ('t, 'u) map (self: 't option) (f: 't -> 'u): 'u option =
  case self of
    None => None
  | Some x => Some (f x)

fun 't unwrap (self: 't option): 't =
  case self of
    None => raise Unwrap
  | Some x => x

fun 't unwrap_or (self: 't option) (default: 't): 't =
  case self of
    None => default
  | Some x => x

fun 't unwrap_or_else (self: 't option) (f: unit -> 't): 't =
  case self of
    None => f ()
  | Some x => x

fun 't is_some (self: 't option): bool =
  case self of
    None => false
  | Some _ => true

fun 't is_none (self: 't option): bool =
  case self of
    None => true
  | Some _ => false

fun ('t, 'u) and_then (self: 't option) (f: 't -> 'u option): 'u option =
  case self of
    None => None
  | Some x => f x

(* forall x, flatten x = and_then x id *)
fun 't flatten (self: 't option option): 't option =
  case self of
    None => None
  | Some None => None
  | Some (Some x) => Some x
"#,
  );
}

#[test]
fn result() {
  check(
    r#"
(* Rust's Result<T, E> *)

datatype ('t, 'e) result = Ok of 't | Err of 'e
exception Unwrap

fun ('t, 'e, 'u) map (self: ('t, 'e) result) (f: 't -> 'u): ('u, 'e) result =
  case self of
    Ok x => Ok (f x)
  | Err e => Err e

fun ('t, 'e) unwrap (self: ('t, 'e) result): 't =
  case self of
    Ok x => x
  | Err _ => raise Unwrap

fun ('t, 'e) unwrap_or (self: ('t, 'e) result) (default: 't): 't =
  case self of
    Ok x => x
  | Err _ => default

fun ('t, 'e) unwrap_or_else (self: ('t, 'e) result) (f: unit -> 't): 't =
  case self of
    Ok x => x
  | Err _ => f ()

fun ('t, 'e) is_ok (self: ('t, 'e) result): bool =
  case self of
    Ok _ => true
  | Err _ => false

fun ('t, 'e) is_err (self: ('t, 'e) result): bool =
  case self of
    Ok _ => false
  | Err _ => true

fun ('t, 'e, 'u) and_then (self: ('t, 'e) result) (f: 't -> ('u, 'e) result): ('u, 'e) result =
  case self of
    Ok x => f x
  | Err e => Err e

(* forall x, flatten x = and_then x id *)
fun ('t, 'e) flatten (self: (('t, 'e) result, 'e) result): ('t, 'e) result =
  case self of
    Ok (Ok x) => Ok x
  | Ok (Err e) => Err e
  | Err e => Err e
"#,
  );
}

#[test]
fn same_fixity_diff_assoc() {
  check(
    r#"
infix <<
infixr >>
val _ = 1 << 2 >> 3
(**            ^^ consecutive infix names with same fixity but different associativity *)
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
fn unclosed_string_constant() {
  check(
    r#"
val _ = "bad
(**     ^^^^ unclosed string literal *)
"#,
  );
}

#[test]
fn undefined() {
  check(
    r#"
val _ = nope
(**     ^^^^ undefined value: nope *)
"#,
  );
}

#[test]
fn unknown_byte() {
  check(
    r#"
val 空条承太郎 = 1
(** ^^^ invalid source character *)
"#,
  );
}

#[test]
fn unmatched_close_comment() {
  check(
    r#"
val x = 3 *)
(**       ^^ unmatched close comment *)
"#,
  );
}

#[test]
fn unmatched_open_comment() {
  check(
    r#"
(**       vv unmatched open comment *)
val x = 3 (*
"#,
  );
}

#[test]
fn unreachable_pattern() {
  check(
    r#"
val _ =
  case 3 of
    4 => 1
  | 4 => 2
(** ^ unreachable pattern *)
  | _ => 3
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
fn nonfix() {
  check(
    r#"
nonfix + = *

val _: bool = = (1, 2)
val _: int = + (1, 2)
val _: int = * (3, 4)
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
