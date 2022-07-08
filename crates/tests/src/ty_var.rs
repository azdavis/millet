use crate::check::check;

#[test]
fn across_var() {
  check(
    r#"
fun 'a f (x: 'a) = let val y = x in y false; y end
(**                                 ^ expected a function type, got 'a *)
"#,
  );
}

#[test]
fn bound_at_fun_1() {
  check(
    r#"
fun bar (x: int): unit = ()
fun 'a f (id: 'a -> 'a) x = bar (id x)
(**                              ^^^^ expected int, found 'a *)
"#,
  );
}

#[test]
fn bound_at_fun_2() {
  check(
    r#"
fun 'a f (id: 'a -> 'a) x = id x + 1
(**                         ^^^^^^^^ expected ?a * ?a with one of {word, real, int}, found 'a * int *)
"#,
  );
}

#[test]
fn annotate() {
  check(
    r#"
val 'a _ = false: 'a
(**        ^^^^^^^^^ expected 'a, found bool *)
"#,
  );
}

#[test]
fn type_datatype() {
  check(
    r#"
type 'a heh = 'a list
datatype 'a bad = Bad of 'a
val _: int heh = [1]
val _ = Bad: unit
(**     ^^^^^^^^^ expected unit, found ?a -> ?a bad *)
"#,
  );
}

#[test]
fn apply() {
  check(
    r#"
fun ('t, 'u) apply (f: 't -> 'u) (x: 't): 'u = f x
val _ = apply op+ (1, false)
(**               ^^^^^^^^^^ expected ?a * ?a, found int * bool *)
"#,
  );
}

#[test]
fn different_vars() {
  check(
    r#"
fun ('a, 'b) f (xs: 'a list) (x: 'b) =
    x :: xs
(** ^^^^^^^ expected ?a * ?a list, found 'b * 'a list *)
"#,
  );
}

#[test]
fn implicit_scope_val() {
  check(
    r#"
fun id (x: 'a): 'a = x
val _ = id 3
val _ = id "hey"
val _: 'a list = []
fun map (f: 'a -> 'b) (xs: 'a list): 'b list =
  case xs of
    [] => []
  | x :: xs => f x :: map f xs
"#,
  );
}

#[test]
fn implicit_scope_spec() {
  check(
    r#"
signature FUNCTOR = sig
  type 'a f
  val map : ('a -> 'b) -> 'a f -> 'b f
end
signature APPLICATIVE = sig
  type 'a f
  val pure : 'a -> 'a f
  val <*> : ('a -> 'b) f -> 'a f -> 'b f
end
signature MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val >>= : ('a -> 'b m) -> 'a m -> 'b m
end
"#,
  );
}
