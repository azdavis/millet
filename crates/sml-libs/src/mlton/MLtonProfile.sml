signature MLTON_PROFILE = sig
  structure Data : sig
    type t
    val equals : t * t -> bool
    val free : t -> unit
    val malloc : unit -> t
    val write : t * string -> unit
  end
  val isOn : bool
  val withData : Data.t * (unit -> 'a) -> 'a
end
