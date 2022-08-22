signature REF = sig
  (*!
   * The ref type, also available at the top level.
   *)
  datatype ref = datatype ref
  (*!
   * `! r` returns the current value in `r`.
   *)
  val ! : 'a ref -> 'a
  (*!
   * `:= (r, x)` sets `x` to be the new value in `r`.
   *)
  val := : 'a ref * 'a -> unit
  (*!
   * `exchange (r, x)` sets `x` to be the new value in `r`, and returns the old value.
   *)
  val exchange : 'a ref * 'a -> 'a
  (*!
   * `swap (r1, r2)` swaps the values of the two refs.
   *)
  val swap : 'a ref * 'a ref -> unit
  (*!
   * `app f r` applies `f` to the value in `r`.
   *)
  val app : ('a -> unit) -> 'a ref -> unit
  (*!
   * `map f r` returns a ref containing `f (!r)`.
   *)
  val map : ('a -> 'b) -> 'a ref -> 'b ref
  (*!
   * `modify f r` updates `r` to contain `f (!r)`.
   *)
  val modify : ('a -> 'a) -> 'a ref -> unit
end

structure Ref :> REF = struct end
