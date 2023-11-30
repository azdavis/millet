//! Common examples that come up a lot.

use crate::check::check;

#[test]
fn apply() {
  check(
    r"
val apply = fn (f, x) => f x
val _ = apply: unit
(**     ^^^^^^^^^^^ expected `unit`, found `(_ -> _) * _ -> _` *)
",
  );
}

#[test]
fn cps() {
  check(
    r"
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
(** ^^^^^^^^^^^^^^^^^^^ expected `unit`, found `_ tree -> (_ -> bool) -> (_ -> _) -> (unit -> _) -> _` *)
",
  );
}

#[test]
fn fact() {
  check(
    r"
fun fact (0 : int) : int = 1
  | fact n = n * fact (n - 1)
",
  );
}

#[test]
fn list_fns() {
  check(
    r"
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
",
  );
}

#[test]
fn list_map() {
  check(
    r"
fun map f xs =
  case xs of
    [] => []
  | x :: xs => f x :: map f xs

val _ = map: unit
(**     ^^^^^^^^^ expected `unit`, found `(_ -> _) -> _ list -> _ list` *)
",
  );
}

#[test]
fn id() {
  check(
    r#"
val id = fn x => x
val a = id 3
val b = id "hey"
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
(**         ^^^^^^ expected `int`, found `string` *)
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
val _ = ref
and f = op+
val _ = f (a, 2)
and _: real = b / b
and _: int = 3 div 0
"#,
  );
}

#[test]
fn curry() {
  check(
    r"
fun curry f x y = f (x, y)

val eq : int -> int -> bool = curry op=
val add = curry op+
val mul = curry op*

val _: bool = eq 1 2
val _: int = add 3 4
val _: int = mul 5 6

val f = add 7
val g = mul 5
val _: bool = eq (f 3) (g 5)
",
  );
}

#[test]
fn either() {
  check(
    r"
datatype ('a, 'b) either = INL of 'a | INR of 'b
val _ = INL 3 : (int, unit) either
val _ = INR 3 : (unit, int) either
",
  );
}
