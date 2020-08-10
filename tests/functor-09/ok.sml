signature SIG = sig
  type t
  val x: t
  val f: t -> t
end

functor Id (S: SIG) = S

structure A = struct
  type t = int
  val x = 3
  fun f y = y
end

structure B = Id (A)
structure C = Id (A)
structure D = Id (B)

val _ = B.f (A.f A.x)
val _ = C.f (B.f A.x)
val _ = D.f (C.f A.x)
val _ = A.f (D.f A.x)

val _ = B.f (A.f B.x)
val _ = C.f (B.f B.x)
val _ = D.f (C.f B.x)
val _ = A.f (D.f B.x)

val _ = B.f (A.f C.x)
val _ = C.f (B.f C.x)
val _ = D.f (C.f C.x)
val _ = A.f (D.f C.x)

val _ = B.f (A.f D.x)
val _ = C.f (B.f D.x)
val _ = D.f (C.f D.x)
val _ = A.f (D.f D.x)
