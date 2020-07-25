signature SIG = sig
  type t
  val foo: t
  val bar: int -> t
  val quz: t -> int
  val cmb: t -> t -> t
end

functor F (A: SIG) = struct
  type heh = A.t list
  val what: A.t = A.cmb A.foo A.foo
  fun hm (xs: heh): A.t =
    case xs of
      [] => A.foo
    | x :: xs => A.cmb (A.bar (A.quz x + 3)) (hm xs)
end

structure Impl = struct
  type t = string
  val foo = "heh"
  fun bar _ = "no"
  fun quz _ = 3
  fun cmb _ y = y
end

structure S = F (Impl)

val xs: S.heh = ["yes", Impl.foo, S.what]
val guh: Impl.t = S.hm xs
val done: string list = [guh, Impl.foo, S.what, "why"]
