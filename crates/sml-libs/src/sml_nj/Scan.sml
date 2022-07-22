signature SCAN = sig
  datatype fmt_item =
    ATOM of Atom.atom
  | LINT of LargeInt.int
  | INT of Int.int
  | LWORD of LargeWord.word
  | WORD of Word.word
  | WORD8 of Word8.word
  | BOOL of bool
  | CHR of char
  | STR of string
  | REAL of Real.real
  | LREAL of LargeReal.real
  | LEFT of (int * fmt_item)
  | RIGHT of (int * fmt_item)
  exception BadFormat
  val sscanf : string -> string -> fmt_item list option
  val scanf : string -> (char, 'a) StringCvt.reader -> (fmt_item list, 'a) StringCvt.reader
end

structure Scan : SCAN = struct end
