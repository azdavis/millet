signature MLTON_RANDOM = sig
  val alphaNumChar : unit -> char
  val alphaNumString : int -> string
  val rand : unit -> word
  val seed : unit -> word option
  val srand : word -> unit
  val useed : unit -> word option
end
