datatype ab = A | B
datatype cd = C | D
val x =
  case (A, C, A) of
    (A, C, _) => 0
  | (B, _, _) => 1
  | (_, _, A) => 4
