signature MLTON_VECTOR = sig
  val create : int -> {done : unit -> 'a vector, sub : int -> 'a, update : int * 'a -> unit}
  val unfoldi : int * 'b * (int * 'b -> 'a * 'b) -> 'a vector * 'b
end
