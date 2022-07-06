signature BIT_ARRAY = sig
  include MONO_ARRAY
  val fromString : string -> array option
  val bits : (int * int list) -> array
  val getBits : array -> int list
  val toString : array -> string
  val isZero : array -> bool
  val extend0 : (array * int) -> array
  val extend1 : (array * int) -> array
  val eqBits : (array * array) -> bool
  val equal : (array * array) -> bool
  val andb : (array * array * int) -> array
  val orb : (array * array * int) -> array
  val xorb : (array * array * int) -> array
  val notb : array -> array
  val << : (array * word) -> array
  val >> : (array * word) -> array
  val setBit : (array * int) -> unit
  val clrBit : (array * int) -> unit
  val union : array -> array -> unit
  val intersection : array -> array -> unit
  val complement : array -> unit
  val lshift : (array * int) -> array
  val rshift : (array * int) -> array
end

structure BitArray :> BIT_ARRAY = struct end
