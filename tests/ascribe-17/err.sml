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
