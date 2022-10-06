structure JSONStreamParser : sig
  type source
  type 'ctx callbacks = {
    null : 'ctx -> 'ctx,
    boolean : 'ctx * bool -> 'ctx,
    integer : 'ctx * IntInf.int -> 'ctx,
    float : 'ctx * real -> 'ctx,
    string : 'ctx * string -> 'ctx,
    startObject : 'ctx -> 'ctx,
    objectKey : 'ctx * string -> 'ctx,
    endObject : 'ctx -> 'ctx,
    startArray : 'ctx -> 'ctx,
    endArray : 'ctx -> 'ctx,
    error : 'ctx * string -> unit
  }
  val openStream : TextIO.instream -> source
  val openFile : string -> source
  val openString : string -> source
  val close : source -> unit
  val parse : 'ctx callbacks -> (source * 'ctx) -> 'ctx
  val parseFile : 'ctx callbacks -> (string * 'ctx) -> 'ctx
end = struct end
