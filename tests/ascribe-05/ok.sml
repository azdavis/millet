structure S: sig
  val x: int
  val y: bool
end = struct
  val x = 3
  val y = false
  val z = "hidden"
end
val _ = S.x
val _ = S.y
