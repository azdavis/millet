fun ('t, 'u) apply (f: 't -> 'u) (x: 't): 'u = f x
val _ = apply op+ (1, false)
