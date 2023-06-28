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
fn unsolved_general() {
  check(
    r#"
fun id x = x
(**        ^ hover: ?a *)
"#,
  );
}

#[test]
fn unsolved_equality() {
  check(
    r#"
fun eq x y = x = y
(**          ^ hover: ??a *)
"#,
  );
}

#[test]
fn unsolved_overload() {
  check(
    r#"
fun add x y = x + y
(**           ^ hover: <num> *)
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
  // to fix this test, we'd need to change how to report types to consider all of the unsolved type
  // variables in the type that 'contains' the type of the thing we're hovering. but that notion
  // might be hard to define.
  //
  // really, it might be better to not report unsolved types (like ?a or ?b) at all, and try to
  // solve types down to their fully-known, generalized forms.
  fail(
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
  // mixing non-generalized fix ty vars with generalized bound ty vars can be problematic.
  fail(
    r#"
fun foo (x : 'a) =
  let
    fun bar y = (x, y)
  in
    bar
(** ^^^ hover: 'b -> 'a * 'b *)
  end
"#,
  );
}
