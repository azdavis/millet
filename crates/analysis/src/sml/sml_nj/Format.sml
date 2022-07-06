structure Format : sig
  datatype fmt_item = ATOM of Atom.atom | LINT of LargeInt.int | INT of Int.int | LWORD of LargeWord.word | WORD of Word.word | WORD8 of Word8.word | BOOL of bool | CHR of char | STR of string | REAL of Real.real | LREAL of LargeReal.real | LEFT of (int * fmt_item) | RIGHT of (int * fmt_item)
  exception BadFormat
  exception BadFmtList
  val format : string -> fmt_item list -> string
  val formatf : string -> (string -> unit) -> fmt_item list -> unit
end = struct end
