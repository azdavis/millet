signature EITHER = sig
  datatype ('a, 'b) either = INL of 'a | INR of 'b
  val isLeft : ('a, 'b) either -> bool
  val isRight : ('a, 'b) either -> bool
  val asLeft : ('a, 'b) either -> 'a option
  val asRight : ('a, 'b) either -> 'b option
  val map : ('a -> 'aa) * ('b -> 'bb) -> ('a, 'b) either -> ('aa, 'bb) either
  val app : ('a -> unit) * ('b -> unit) -> ('a, 'b) either -> unit
  val fold : ('a * 'b -> 'b) * ('b * 'b -> 'b) -> 'b -> ('a, 'b) either -> 'b
  val proj : ('a, 'a) either -> 'a
  val partition : (('a, 'b) either) list -> ('a list * 'b list)
  val mapLeft : ('a -> 'aa) -> ('a, 'b) either -> ('aa, 'b) either
  val mapRight : ('b -> 'bb) -> ('a, 'b) either -> ('a, 'bb) either
  val appLeft : ('a -> unit) -> ('a, 'b) either -> unit
  val appRight : ('b -> unit) -> ('a, 'b) either -> unit
end

structure Either :> EITHER = struct end
