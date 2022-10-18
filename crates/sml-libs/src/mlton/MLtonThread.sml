signature MLTON_THREAD = sig
  structure AtomicState : sig
    datatype t = NonAtomic | Atomic of int
  end
  val atomically : (unit -> 'a) -> 'a
  val atomicBegin : unit -> unit
  val atomicEnd : unit -> unit
  val atomicState : unit -> AtomicState.t
  structure Runnable : sig
    type t
  end
  type 'a t
  val atomicSwitch : ('a t -> Runnable.t) -> 'a
  val new : ('a -> unit) -> 'a t
  val prepend : 'a t * ('b -> 'a) -> 'b t
  val prepare : 'a t * 'a -> Runnable.t
  val switch : ('a t -> Runnable.t) -> 'a
end

(* NOTE not sure if this actually exists, but other MLton signatures use it *)
structure Thread : MLTON_THREAD = struct end
