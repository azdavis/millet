use crate::check::check;

#[test]
fn apply() {
  check(
    r#"
val apply = fn (f, x) => f x
    val _: unit = apply
(** ^^^^^^^^^^^^^^^^^^^ mismatched types: expected unit, found (_ -> _) * _ -> _ *)
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
(** ^^^^^^^ mismatched types: expected unit, found _ *)
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
(** ^^^ non-exhaustive binding *)
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
#[ignore = "todo for new"]
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
(** ^^^^^^^^^^^^^^^^^^^ mismatched types: expected unit, found '46 tree -> ('46 -> bool) -> ('46 -> '45) -> (unit -> '45) -> '45 *)
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
#[ignore = "todo for new"]
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
#[ignore = "todo for new"]
fn forbidden_binding() {
  check(
    r#"
datatype no = ref
(**           ^^^ forbidden identifier in binding: ref *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
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
#[ignore = "todo for new"]
fn fun_dec_name_mismatch() {
  check(
    r#"
fun f 1 = 1
  | g _ = 2
(** ^ mismatched names: expected f, found g *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn fun_dec_wrong_num_pats() {
  check(
    r#"
fun f 1 = 2
  | f 3 4 = 5
(**   ^^^ mismatched number of patterns: expected 1, found 2 *)
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
(** ^^^^^^ mismatched types: expected int, found int * int *)
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
#[ignore = "should not error"]
fn implicit_ty_var_scope() {
  check(
    r#"
fun id (x: 'a): 'a = x
val _ = id 3
val _ = id "hey"
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn inc() {
  check(
    r#"
val inc = fn x => x + 1
val _ = inc 3
val _ = inc "nope"
(**     ^^^^^^^^^^ mismatched types: expected int, found string *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn infix_without_op() {
  check(
    r#"
val _ = + (1, 2)
(**     ^ infix name used as non-infix without `op` *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn many_vars() {
  check(
    r#"
fun go a b c d e f g =
  if a > 3 then b else go e b (b c d) d (a + 1) (g f) g
    val _: unit = go
(** ^^^^^^^^^^^^^^^^ mismatched types: expected unit, found int -> ('46 -> '45 -> '46) -> '46 -> '45 -> int -> '47 -> ('47 -> '47) -> '46 -> '45 -> '46 *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn map() {
  check(
    r#"
datatype 'a option = None | Some of 'a

fun option_map f opt =
  case opt of
    None => None
  | Some x => Some (f x)

fun list_map f xs =
  case xs of
    [] => []
  | x :: xs => f x :: list_map f xs

    val _: unit = (option_map, list_map)
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ mismatched types: expected unit, found (('52 -> '53) -> '52 option -> '53 option) * (('54 -> '55) -> '54 list -> '55 list) *)
"#,
  );
}

#[test]
fn negative_fixity() {
  check(
    r#"
infix ~3 bad
(**   ^^ fixity is negative *)
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
#[ignore = "todo for new"]
fn non_exhaustive_binding() {
  check(
    r#"
val 3 = 1 + 2
(** ^ non-exhaustive binding *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn non_var_in_as() {
  check(
    r#"
exception Bad
val _ =
  case 3 of
    Bad as _ => 1
(** ^^^ pattern to left of `as` is not a variable: Bad *)
  | _ => 2
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn not_arrow_ty() {
  check(
    r#"
val _ = 3 3
(**     ^^^ not a function type: int *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn not_equality() {
  check(
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
#[ignore = "todo for new"]
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
#[ignore = "todo for new"]
fn ty_mismatch() {
  check(
    r#"
fun f x = x + 1
val _ = f false
(**     ^^^^^^^ mismatched types: expected int, found bool *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn ty_var_scope() {
  check(
    r#"
val _ = fn id =>
  (id 3; id "nope")
(**      ^^^^^^^^^ mismatched types: expected int, found string *)
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
val _ = x
(**     ^ undefined value: x *)
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
#[ignore = "todo for new"]
fn useless_ty_var() {
  check(
    r#"
fun 'a f () = 3
    val _: unit = f
(** ^^^^^^^^^^^^^^^ mismatched types: expected unit, found unit -> int *)
"#,
  );
}

#[test]
#[ignore = "should error"]
fn value_restriction() {
  check(
    r#"
    val id = (fn x => x) (fn x => x)
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ oh no! *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn wrong_num_ty_args() {
  check(
    r#"
val _: (int, bool) list = []
(**    ^^^^^^^^^^^^^^^^ mismatched number of type arguments: expected 1, found 2 *)
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
