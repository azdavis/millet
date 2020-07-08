datatype t = C of int * bool
val C (a, b) = C (1, false)
val _ = if b then a + 2 else a - 4
