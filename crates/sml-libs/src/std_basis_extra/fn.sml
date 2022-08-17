signature FN = sig
  (*!
  `id x` returns `x`.

  This is the identity function.
  !*)
  val id : 'a -> 'a
  (*!
  `const x y` returns `x`.

  Put another way, `const x` returns a function which takes anything and always returns `x`.

  Note that because SML is not lazy by default, the argument `y` is fully evaluated.
  !*)
  val const : 'a -> 'b -> 'a
  (*!
  `apply (f, x)` returns `f x`.
  !*)
  val apply : ('a -> 'b) * 'a -> 'b
  (*!
  `o (f, g) x` returns `g (f x)`.

  Put another way, `o (f, g)` returns a function that is the composition of `f` after `g`.
  !*)
  val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
  (*!
  `curry f x y` returns `f (x, y)`.

  Put another way, `curry f` returns a function `f'` that is identical to `f`, except `f'` takes its
  arguments "curried". That is:

  - `f` takes a 2-tuple as its argument and returns the result.
  - `f'` takes the first element of that 2 tuple as its argument, and returns a function. That
    function takes the second element, and returns the result.
  !*)
  val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  (*!
  `uncurry f (x, y)` returns `f x y`.

  Put another way, `uncurry f` returns a function `f'` that is identical to `f`, except `f'` takes
  its arguments "uncurried". That is:

  - `f` takes an argument, then returns a function. That function takes another argument, then
    returns the result.
  - `f;` takes a 2-tuple of both arguments, then returns the result.
  !*)
  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
  (*!
  `flip f (y, x)` returns `f (x, y)`.

  Put another way, `flip f` returns a function `f'` that is identical to `f`, except it flips the
  order of its arguments (taken as a 2-tuple).
  !*)
  val flip : ('a * 'b -> 'c) -> ('b * 'a -> 'c)
  (*!
  `repeat n f x` returns `f (f (... (f x))...)`, where there are `n` repetitions of `f`.
  !*)
  val repeat : int -> ('a -> 'a) -> ('a -> 'a)
  (*!
  `equal x y` returns `x = y`.
  !*)
  val equal : ''a -> ''a -> bool
  (*!
  `notEqual x y` returns `x <> y`.
  !*)
  val notEqual : ''a -> ''a -> bool
end

structure Fn :> FN = struct end
