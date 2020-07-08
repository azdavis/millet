val _: int = 3 + 3
and _: real = 3.3 + 3.3
(* what's this? *)
and _: word = 0w0 + 0w0
and _: bool list = [3 > 3]
and _ = "derp" > "doot"
val (a, b: real) = (123, 2.34)
val _ = #"e" > (#"f": char)
val _ = 3 = 4
val _ = if 123 < 234 then [LESS] else [GREATER, EQUAL]
val _ = ref
and f = op+
val _ = f (a, 2)
and _: real = b / b
and _: int = 3 div 0
