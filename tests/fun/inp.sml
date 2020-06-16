infix &&
fun f && g = (f, g)
infix ||
fun (a, b) || (c, d) = 3
infix %%
fun () %% 4 = 5
  | (() %% _) = 6
infix <$>
fun ('a, 'b) (f: 'a -> 'b) <$> ([]: 'a list): 'b list = []
  | f <$> (x :: xs) = f x :: (f <$> xs)
fun inc x = x + 1
val _ = inc <$> [1,2,3]
infix doot
fun (_ doot _) = 123
