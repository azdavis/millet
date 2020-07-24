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
