fun go a b c d e f g =
  if a > 3 then b else go e b (b c d) d (a + 1) (g f) g
val _: unit = go
