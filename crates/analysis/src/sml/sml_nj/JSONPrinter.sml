structure JSONPrinter : sig
  val print : TextIO.outstream * JSON.value -> unit
  val print' : {strm : TextIO.outstream, pretty : bool} -> JSON.value -> unit
end = struct end
