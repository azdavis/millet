signature EITHER = sig
  (*!
   * The either type, with two cases: `INL` and `INR`.
   *)
  datatype ('a, 'b) either = INL of 'a | INR of 'b
  (*!
   * Returns whether this is a left either (`INL`).
   *)
  val isLeft : ('a, 'b) either -> bool
  (*!
   * Returns whether this is a right either (`INR`).
   *)
  val isRight : ('a, 'b) either -> bool
  (*!
   * Returns `SOME x` if this either is `INL x`, otherwise returns `NONE`.
   *)
  val asLeft : ('a, 'b) either -> 'a option
  (*!
   * Returns `SOME x` if this either is `INR x`, otherwise returns `NONE`.
   *)
  val asRight : ('a, 'b) either -> 'b option
  (*!
   * `map (f, g) e` applies either `f` or `g` to the value in `e` and re-wraps the result with
   * whatever case `e` originally was.
   *)
  val map : ('a -> 'aa) * ('b -> 'bb) -> ('a, 'b) either -> ('aa, 'bb) either
  (*!
   * `app (f, g) e` applies either `f` or `g` to the value in `e`.
   *)
  val app : ('a -> unit) * ('b -> unit) -> ('a, 'b) either -> unit
  (*!
   * Uses either the left or right function to combine the value in the either with the starting
   * value.
   *)
  val fold : ('a * 'b -> 'b) * ('b * 'b -> 'b) -> 'b -> ('a, 'b) either -> 'b
  (*!
   * Extracts the value out of an either, regardless of whether it was in the left or right case.
   *
   * This is possible because we know both the left and right cases wrap the same type `'a`, so the
   * result type is also `'a`.
   *)
  val proj : ('a, 'a) either -> 'a
  (*!
   * Puts the left eithers in the left list and the right in the right list, in order.
   *)
  val partition : (('a, 'b) either) list -> ('a list * 'b list)
  (*!
   * Applies the function to a left either and re-wraps the result. If the either was right, returns
   * it unchanged.
   *)
  val mapLeft : ('a -> 'aa) -> ('a, 'b) either -> ('aa, 'b) either
  (*!
   * Applies the function to a right either and re-wraps the result. If the either was left, returns
   * it unchanged.
   *)
  val mapRight : ('b -> 'bb) -> ('a, 'b) either -> ('a, 'bb) either
  (*!
   * Applies the function to a left either. If the either was right, does nothing.
   *)
  val appLeft : ('a -> unit) -> ('a, 'b) either -> unit
  (*!
   * Applies the function to a right either. If the either was left, does nothing.
   *)
  val appRight : ('b -> unit) -> ('a, 'b) either -> unit
end

structure Either :> EITHER = struct end
