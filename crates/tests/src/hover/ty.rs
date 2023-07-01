//! Test for getting type information on hover.

use crate::check::{check, fail};

#[test]
fn smoke() {
  check(
    r#"
val _ = 123
(**     ^ hover: int *)
"#,
  );
}

#[test]
fn tuple() {
  check(
    r#"
datatype 'a uh = Uh of 'a
val _ = (   3, "hi", Uh false)
(**       ^ hover: int * string * bool uh *)
"#,
  );
}

#[test]
fn fully_qualified() {
  check(
    r#"
structure Foo = struct datatype bar = baz end
val _ = Foo.baz
(**     ^ hover: Foo.bar *)
"#,
  );
}

#[test]
fn fun() {
  check(
    r#"
fun foo x y = x + (if y then 3 else 4)
(** ^^^ hover: int -> bool -> int *)
"#,
  );
}

#[test]
fn ty_var_regular() {
  check(
    r#"
fun id x = x
(**        ^ hover: ?a *)
"#,
  );
}

#[test]
fn ty_var_equality() {
  check(
    r#"
fun eq x y = x = y
(**          ^ hover: ??a *)
"#,
  );
}

#[test]
fn ty_var_overload() {
  check(
    r#"
fun add x y = x + y
(**           ^ hover: int *)
"#,
  );
}

/// not the greatest
#[test]
fn unsolved_record() {
  check(
    r#"
(**               vvvv cannot resolve `...` in record type: `{ foo : bool, ... }` *)
fun getFoo x = if #foo x then 3 else 4
(**                    ^ hover: { foo : bool, ... } *)
"#,
  );
}

#[test]
fn swap() {
  check(
    r#"
(**          v hover: ?b *)
fun swap (a, b) = (b, a)
(**       ^ hover: ?a *)
"#,
  );
}

#[test]
fn transparent_alias() {
  check(
    r#"
signature SIG = sig
  type t
  val x : t
end

structure Str : SIG = struct
  type t = unit
  val x = ()
end

val _ = Str.x
(**         ^ hover: unit *)
"#,
  );
}

#[test]
fn transparent_datatype() {
  check(
    r#"
signature SIG = sig
  type t
  val x : t
end

structure Str : SIG = struct
  datatype t = D
  val x = D
end

val _ = Str.x
(**         ^ hover: Str.t *)
"#,
  );
}

#[test]
fn opaque_alias() {
  check(
    r#"
signature SIG = sig
  type t
  val x : t
end

structure Str :> SIG = struct
  type t = unit
  val x = ()
end

val _ = Str.x
(**         ^ hover: Str.t *)
"#,
  );
}

#[test]
fn opaque_datatype() {
  check(
    r#"
signature SIG = sig
  type t
  val x : t
end

structure Str :> SIG = struct
  datatype t = D
  val x = D
end

val _ = Str.x
(**         ^ hover: Str.t *)
"#,
  );
}

#[test]
fn fixed_not_generalized() {
  check(
    r#"
fun foo (x : 'a) =
  let
    fun bar y = (x, y)
  in
    bar
(** ^^^ hover: ?b -> 'a * ?b *)
  end
"#,
  );
}

#[test]
fn nested_generalize() {
  fail(
    r#"
fun foo x =
(**     ^ hover: ?a *)
  let
    fun bar y =
(**         ^ hover: ?b *)
(**    v hover: ?a *)
      (x, y)
(**       ^ hover: ?b *)
  in
    bar
  end
"#,
  );
}
