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
