datatype d = A | B of int
val _ =
  case A of
    A _ => 1
  | B _ => 2
