use crate::check::check;

#[test]
fn ok_smoke() {
  check(
    r#"
structure S: sig
  val x: int
end = struct
  val x = 3
end
val _: int = S.x
"#,
  );
}

#[test]
fn not_in_sig() {
  check(
    r#"
structure S: sig
end = struct
  val x = 3
end
val _: int = S.x
(**          ^^^ undefined value: x *)
"#,
  );
}

#[test]
fn not_in_struct() {
  check(
    r#"
structure S: sig
  val x: int
end = struct end
(**   ^^^^^^^^^^ missing value required by signature: x *)
"#,
  );
}

#[test]
fn wrong_id_status() {
  check(
    r#"
exception Foo

structure S: sig
  exception E
end = struct val E = Foo end
(**   ^^^^^^^^^^^^^^^^^^^^^^ incompatible identifier statuses: E *)
"#,
  );
}

#[test]
fn ok_hide_some() {
  check(
    r#"
structure S: sig
  val x: int
  val y: bool
end = struct
  val x = 3
  val y = false
  val z = "hidden"
end
val _ = S.x
val _ = S.y
"#,
  );
}

#[test]
fn datatype_ok() {
  check(
    r#"
structure S: sig
  datatype d = A | B
end = struct
  datatype d = A | B
end
"#,
  );
}

#[test]
fn datatype_missing_ctor() {
  check(
    r#"
structure S: sig
  datatype d = A | B
end = struct datatype d = A end
(**   ^^^^^^^^^^^^^^^^^^^^^^^^^ missing value required by signature: B *)
"#,
  );
}

#[test]
fn datatype_extra_ctor() {
  check(
    r#"
structure S: sig
  datatype d = A
end = struct datatype d = A | B end
(**   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ extra value not present in signature: B *)
"#,
  );
}

#[test]
fn type_ok() {
  check(
    r#"
structure S: sig
  type t
end = struct
  type t = int
end
val _: S.t = 3
"#,
  );
}

#[test]
fn monoid_transparent() {
  check(
    r#"
structure S: sig
  type t
  val zero: t
  val add: t -> t -> t
end = struct
  type t = int
  val zero = 0
  fun add x y = x + y
end
val _: S.t = 3
val _ = S.add S.zero S.zero
val _ = S.add 1 S.zero
val _ = S.add S.zero 2
val _ = S.add 1 2
fun inc (x: int): int = x + 1
val _ = inc S.zero
val _ = S.add (S.add (inc S.zero) (inc S.zero))
"#,
  );
}

#[test]
fn sig_bind_smoke() {
  check(
    r#"
signature SIG = sig
  type t
end
structure S: SIG = struct
  type t = int
end
"#,
  );
}

#[test]
fn monoid_add_mul_transparent() {
  check(
    r#"
signature MONOID = sig
  type t
  val zero: t
  val add: t -> t -> t
end

fun curry f x y = f (x, y)

structure Add: MONOID = struct
  type t = int
  val zero = 0
  val add = curry op+
end

structure Mul: MONOID = struct
  type t = int
  val zero = 1
  val add = curry op*
end

fun isZero eq z f a = eq (f z a) a andalso eq (f a z) a
fun isAssoc eq f a b c = eq (f (f a b) c) (f a (f b c))
fun isMonoid eq z f a b c =
  isZero eq z f a andalso
  isZero eq z f b andalso
  isZero eq z f c andalso
  isAssoc eq f a b c

exception AssertFail
fun assert x = if x then () else raise AssertFail

fun intEq (a: int) (b: int) = a = b

val _ = assert (isMonoid intEq Add.zero Add.add 3 4 5)
val _ = assert (isMonoid intEq Mul.zero Mul.add 3 4 5)

val _ = Add.add Add.zero Add.zero
val _ = Mul.add Mul.zero Mul.zero

(* should pass, since transparent *)
val _ = Mul.add Mul.zero Add.zero
"#,
  );
}

#[test]
fn monoid_opaque() {
  check(
    r#"
signature MONOID = sig
  type t
  val zero: t
  val add: t -> t -> t
end

fun curry f x y = f (x, y)

structure Add:> MONOID = struct
  type t = int
  val zero = 0
  val add = curry op+
end

structure Mul:> MONOID = struct
  type t = int
  val zero = 1
  val add = curry op*
end

val _ = Add.add Add.zero Add.zero
val _ = Mul.add Mul.zero Mul.zero

(* TODO improve error message with FQN *)
val _ = Mul.add Mul.zero Add.zero
(**                      ^^^^^^^^ expected t, found t *)
"#,
  );
}

#[test]
fn textually_identical_but_not_same() {
  check(
    r#"
signature SIG = sig
  type t
  val foo: t
  val bar: t -> unit
end

structure A:> SIG = struct
  type t = int
  val foo = 3
  fun bar _ = ()
end

structure B:> SIG = struct
  type t = int
  val foo = 3
  fun bar _ = ()
end

val _ = A.bar A.foo = B.bar B.foo

val _ = A.bar B.foo
(**           ^^^^^ expected t, found t *)
"#,
  );
}

#[test]
fn same_identifier_opaque_1() {
  check(
    r#"
signature SIG = sig
  type t
  val foo: t
  val bar: t -> unit
end

structure S = struct
  type t = int
  val foo = 3
  fun bar _ = ()
end

structure A:> SIG = S

fun inc (x: int) = x + 1

val _ = S.bar S.foo
val _ = S.bar 123
val _ = inc S.foo

val _ = A.bar A.foo
val _ = A.bar 123
(**           ^^^ expected t, found int *)
"#,
  );
}

#[test]
fn same_identifier_opaque_2() {
  check(
    r#"
signature SIG = sig
  type t
  val foo: t
  val bar: t -> unit
end

structure S = struct
  type t = int
  val foo = 3
  fun bar _ = ()
end

structure A:> SIG = S
structure B:> SIG = S

fun inc (x: int) = x + 1

val _ = S.bar S.foo
val _ = S.bar 123
val _ = inc S.foo

val _ = A.bar A.foo
val _ = B.bar B.foo
val _ = B.bar A.foo
(**           ^^^^^ expected t, found t *)
"#,
  );
}

#[test]
fn same_identifier_opaque_3() {
  check(
    r#"
signature SIG = sig
  type t
  val x: t
end

structure Str = struct
  type t = int
  val x = 3
end

structure A: SIG = Str
structure B: SIG = Str

val _: A.t = B.x

structure C:> SIG = Str
structure D:> SIG = Str

val _ = D.x: C.t
(**     ^^^^^^^^ expected t, found t *)
"#,
  );
}

#[test]
fn opaque_hides_type() {
  check(
    r#"
signature SIG = sig
  type t
  val x: t
end

structure S:> SIG = struct
  type t = int
  val x = 3
end

val _: S.t = S.x
val _ = S.x: int
(**     ^^^^^^^^ expected int, found t *)
"#,
  );
}

#[test]
fn transparent_shows_type() {
  check(
    r#"
signature SIG = sig
  type t
  val x: t
end

structure S: SIG = struct
  type t = int
  val x = 3
end

val _: S.t = S.x
val _: int = S.x
"#,
  );
}

#[test]
fn functor_sig() {
  check(
    r#"
signature FUNCTOR = sig
  type 'a f
  val map: ('a -> 'b) -> 'a f -> 'b f
end

structure ListFunctor: FUNCTOR = struct
  type 'a f = 'a list
  fun map f xs =
    case xs of
      [] => []
    | x :: xs => f x :: map f xs
end

datatype 'a option = NONE | SOME of 'a

structure OptionFunctor: FUNCTOR = struct
  type 'a f = 'a option
  fun map f x =
    case x of
      NONE => NONE
    | SOME x => SOME (f x)
end
"#,
  );
}

#[test]
fn where_in_functor() {
  check(
    r#"
signature T = sig type t end
functor Id (X : T) :> T where type t = int = X
(**                                          ^ expected int, found t *)
"#,
  );
}

#[test]
fn where_with_self() {
  check(
    r#"
signature T = sig type t end
functor Id (X : T) :> T where type t = X.t = X
"#,
  );
}

#[test]
fn where_opaque() {
  check(
    r#"
signature SIG = sig
  type t
  val x : t
end

structure S :> SIG where type t = int = struct
  type t = int
  val x = 3
end

val _ = S.x : int
"#,
  );
}

#[test]
fn exn() {
  check(
    r#"
signature SIG = sig
  exception E
end

structure Str :> SIG = struct
  exception E
end
"#,
  );
}

#[test]
fn structure_spec() {
  check(
    r#"
signature INNER = sig
  type t
  val x : t
end

signature OUTER = sig
  structure A : INNER
  structure B : INNER
end

functor Id (X : OUTER) :> OUTER = X
"#,
  );
}

#[test]
fn sig_ty_alias_1() {
  check(
    r#"
signature SIG = sig
  type 'a t
  type 'a u = (unit * 'a) t
  val no : 'a u
end
structure Str :> SIG = struct
  type 'a t = 'a -> unit
  type 'a u = (unit * 'a) t
  val no = fn ((), _) => ()
end
"#,
  );
}

/// the same as [`sig_ty_alias_1`] but with the t and u swapped. these 2 tests check for
/// questionable things involving iteration order.
#[test]
fn sig_ty_alias_2() {
  check(
    r#"
signature SIG = sig
  type 'a u
  type 'a t = (unit * 'a) u
  val no : 'a t
end
structure Str :> SIG = struct
  type 'a u = 'a -> unit
  type 'a t = (unit * 'a) u
  val no = fn ((), _) => ()
end
"#,
  );
}
