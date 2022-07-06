structure JSONParser : sig
  type source
  val openStream : TextIO.instream -> source
  val openFile : string -> source
  val openString : string -> source
  val close : source -> unit
  val parse : source -> JSON.value
  val parseFile : string -> JSON.value
end = struct end
