functor Id (S: sig end) = S

structure Guy = Id (struct
  val x = 3
end)

(* nope *)
val _: int = Guy.x
