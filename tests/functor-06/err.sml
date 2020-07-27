signature SIG = sig
  val x: int
end

functor F (A: SIG): sig end = A

structure S = F (struct
  val x = 3 end
)

val _ = S.x
