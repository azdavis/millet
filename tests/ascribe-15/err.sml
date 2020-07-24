signature SIG = sig
  type t
  val make: unit -> t
  val doSomething: t -> unit
end

structure A:> SIG = struct
  type t = int
  fun make () = 3
  fun doSomething _ = ()
end

(* note that the `struct` body is textually identical to A... *)
structure B:> SIG = struct
  type t = int
  fun make () = 3
  fun doSomething _ = ()
end

val _ = A.doSomething (A.make ())
val _ = B.doSomething (B.make ())

(* ...nonetheless, this should fail *)
val _ = A.doSomething (B.make ())
