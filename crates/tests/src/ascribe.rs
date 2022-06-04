use crate::check::check;

#[test]
fn t_01() {
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
fn t_02() {
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
fn t_03() {
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
fn t_04() {
  check(
    r#"
structure S: sig
  exception E
end = struct val E = Match end
(**   ^^^^^^^^^^^^^^^^^^^^^^^^ incompatible identifier statuses: E *)
"#,
  );
}

#[test]
fn t_05() {
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
fn t_06() {
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
fn t_07() {
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
fn t_08() {
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
fn t_09() {
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
fn t_10() {
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
fn t_11() {
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
fn t_12() {
  check(
    r#"
structure S: sig
  type t
end = struct
  type t = int
end
"#,
  );
}

#[test]
fn t_13() {
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
fn t_14() {
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

(* TODO not the best error message *)
val _ = Mul.add Mul.zero Add.zero
(**                      ^^^^^^^^ mismatched types: expected t, found t *)
"#,
  );
}

#[test]
fn t_15() {
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

(* note that the `struct` body is textually identical to A... *)
structure B:> SIG = struct
  type t = int
  val foo = 3
  fun bar _ = ()
end

val _ = A.bar A.foo = B.bar B.foo

(* ...nonetheless, this should fail *)
val _ = A.bar B.foo
(**           ^^^^^ mismatched types: expected t, found t *)
"#,
  );
}

#[test]
fn t_16() {
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
(**           ^^^ mismatched types: expected t, found int *)
"#,
  );
}

#[test]
fn t_17() {
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
(**           ^^^^^ mismatched types: expected t, found t *)
"#,
  );
}

#[test]
fn t_18() {
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
    val _: int = S.x
(** ^^^^^^^^^^^^^^^^ mismatched types: expected int, found t *)
"#,
  );
}

#[test]
fn t_19() {
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
fn t_20() {
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

    val _: C.t = D.x
(** ^^^^^^^^^^^^^^^^ mismatched types: expected t, found t *)
"#,
  );
}
