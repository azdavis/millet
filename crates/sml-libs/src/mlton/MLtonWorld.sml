signature MLTON_WORLD = sig
  datatype status = Clone | Original
  val load : string -> 'a
  val save : string -> status
  val saveThread : string * Thread.Runnable.t -> unit
end
