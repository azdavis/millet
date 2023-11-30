//! Tests for `val rec`, which `fun` lowers to.

use crate::check::check;

#[test]
fn mutual_one_kw() {
  check(
    r"
val rec f = fn x =>
  if x < 5 then
    x
  else
    g (x - 6)
and g = fn y =>
  if y < 10 then
    y
  else
    f (y - 4)
",
  );
}

#[test]
fn mutual_two_kw() {
  check(
    r"
val rec f = fn x =>
  if x < 5 then
    x
  else
    g (x - 6)
and rec g = fn y =>
  if y < 10 then
    y
  else
    f (y - 4)
",
  );
}

#[test]
fn complex_pat() {
  check(
    r"
type num = int

val rec ((f : int -> num as g : int -> int as h) : num -> int) : num -> num = fn x =>
  case x of
    1 => f (x - 1)
  | 2 => g (x - 2)
  | 3 => h (x - 3)
  | _ => x + 4

val _ : int = f (g (h 3))
",
  );
}

#[test]
fn wild_pat() {
  check(
    r"
val rec _ = fn () => ()
",
  );
}

#[test]
fn paren() {
  check(
    r"
val rec f = (fn () => ())
",
  );
}

#[test]
fn exp_ann() {
  check(
    r"
val rec f = (fn () => ()) : unit -> unit
",
  );
}

#[test]
fn let_() {
  check(
    r"
val rec f = let val three = 3 in fn () => three end
(** + was not a `fn` *)
",
  );
}

#[test]
fn handle() {
  check(
    r"
val rec f = (fn () => 1) handle _ => (fn () => 2)
(** + was not a `fn` *)
",
  );
}

#[test]
fn var() {
  check(
    r"
val g = fn () => ()
val rec f = g
(** + was not a `fn` *)
",
  );
}
