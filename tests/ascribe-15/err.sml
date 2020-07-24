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
