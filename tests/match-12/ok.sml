datatype ab = A | B
datatype cd = C | D
val x =
  case (A, C, A) of
    (A, C, A) => 0
  | (B, C, A) => 1
  | (_, _, B) => 3
  | (_, _, A) => 4
