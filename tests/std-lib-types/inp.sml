val _: int = 3 + 3
val _: real = 3.3 + 3.3
(* what's this? *)
val _: word = 0w0 + 0w0
val _: bool list = [3 > 3]
val _ = "derp" > "doot"
val (a, b: real) = (123, 2.34)
val _ = #"e" > (#"f": char)
val _ = 3 = 4
val _ = if 123 < 234 then [LESS] else [GREATER, EQUAL]
val _ = ref
val f = op+
val _ = f (a, 2)
val _: real = b / b
val _: int = 3 div 0
