structure SExpParser : sig
  val parse : TextIO.instream -> SExp.value list
  val parseFile : string -> SExp.value list
end = struct end
