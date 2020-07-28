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
