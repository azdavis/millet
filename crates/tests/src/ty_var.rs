//! Some tests are from [mlton docs](http://mlton.org/TypeVariableScope).

use crate::check::check;

#[test]
fn across_var() {
  check(
    r#"
fun 'a f (x: 'a) = let val y = x in y false; y end
(**                                 ^ expected bool -> ?a, found 'a *)
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
(**                         ^^^^^^^^ expected <num> * <num>, found 'a * int *)
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
(**               ^^^^^^^^^^ expected <num> * <num>, found int * bool *)
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

#[test]
fn implicit_scope_1() {
  check(
    r#"
fun f (_ : 'a list) =
  let
    val _ : 'a list = []
  in
    ()
  end
"#,
  );
}

#[test]
fn implicit_scope_2() {
  check(
    r#"
val _ =
  let
    val _ : 'a list = []
  in
    [] : 'a list
  end
"#,
  );
}

#[test]
fn implicit_scope_3() {
  check(
    r#"
val _ =
  let
    val _ : 'a list = []
  in
    []
  end
"#,
  );
}

#[test]
fn on_val_in_type() {
  check(
    r#"
val 'a foo = let type t = 'a in () end
(**                       ^^ type variable bound at `val` or `fun` not allowed here *)
"#,
  );
}

#[test]
fn on_val_in_datatype() {
  check(
    r#"
val 'a foo = let datatype d = D of 'a in () end
(**                                ^^ type variable bound at `val` or `fun` not allowed here *)
"#,
  );
}

#[test]
fn on_val_in_exn() {
  check(
    r#"
val 'a foo = let exception E of 'a in () end
"#,
  );
}

#[test]
fn on_fun_in_type() {
  check(
    r#"
fun 'a foo () = let type t = 'a in () end
(**                          ^^ type variable bound at `val` or `fun` not allowed here *)
"#,
  );
}

#[test]
fn on_fun_in_datatype() {
  check(
    r#"
fun 'a foo () = let datatype d = D of 'a in () end
(**                                   ^^ type variable bound at `val` or `fun` not allowed here *)
"#,
  );
}

#[test]
fn on_fun_in_exn() {
  check(
    r#"
fun 'a foo () = let exception E of 'a in () end
"#,
  );
}

#[test]
fn definition_ex_1() {
  check(
    r#"
val x : int -> int = let val id : 'a -> 'a = fn z => z in id id end
"#,
  );
}

#[test]
fn definition_ex_2() {
  check(
    r#"
val _ = (let val id : 'a -> 'a = fn z => z in id id end; fn z => z : 'a)
(**                                              ^^ expected 'a, found 'a -> 'a *)
"#,
  );
}

#[test]
fn inner_fun_scope_implicit() {
  check(
    r#"
fun f x = let fun g (y : 'a) = if true then x else y in () end
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type escapes its scope: 'a *)
"#,
  );
}

#[test]
fn inner_fun_scope_explicit() {
  check(
    r#"
fun f x = let fun 'a g (y : 'a) = if true then x else y in () end
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type escapes its scope: 'a *)
"#,
  );
}

#[test]
fn already_in_scope_explicit() {
  check(
    r#"
fun 'a f (x : 'a) =
  let
    fun 'a g (y : 'a) = y
(** ^^^^^^^^^^^^^^^^^^^^^ duplicate type variable: 'a *)
  in
    ()
  end
"#,
  );
}

#[test]
fn already_in_scope_implicit() {
  check(
    r#"
fun f (x : 'a) =
  let
    fun 'a g (y : 'a) = y
(** ^^^^^^^^^^^^^^^^^^^^^ duplicate type variable: 'a *)
  in
    ()
  end
"#,
  );
}
