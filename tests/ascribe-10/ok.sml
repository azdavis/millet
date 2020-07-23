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
