structure JSONStreamPrinter : sig
  type printer
  val new : TextIO.outstream -> printer
  val new' : {strm : TextIO.outstream, pretty : bool} -> printer
  val close : printer -> unit
  val null : printer -> unit
  val boolean : printer * bool -> unit
  val integer : printer * IntInf.int -> unit
  val float : printer * real -> unit
  val string : printer * string -> unit
  val beginObject : printer -> unit
  val objectKey : printer * string -> unit
  val endObject : printer -> unit
  val beginArray : printer -> unit
  val endArray : printer -> unit
end = struct end
