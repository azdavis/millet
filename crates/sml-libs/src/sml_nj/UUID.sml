structure UUID : sig
  type t
  val null : t
  val compare : t * t -> order
  val same : t * t -> bool
  val hash : t -> word
  val toString : t -> string
  val fromString : string -> t option
  val toBytes : t -> Word8Vector.vector
  val fromBytes : Word8Vector.vector -> t
end = struct end
