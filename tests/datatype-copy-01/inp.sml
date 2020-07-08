datatype guh = A | B
datatype heh = datatype guh
fun f (x: heh): int =
  case x of
    A => 1
  | B => 2
val _ = f A + f B
