signature UTF8 = sig
  type wchar = word
  val maxCodePoint : wchar
  exception Incomplete
  val getu : (char, 'strm) StringCvt.reader -> (wchar, 'strm) StringCvt.reader
  val encode : wchar -> string
  val isAscii : wchar -> bool
  val toAscii : wchar -> char
  val fromAscii : char -> wchar
  val toString : wchar -> string
  val size : string -> int
  val explode : string -> wchar list
  val implode : wchar list -> string
  val map : (wchar -> wchar) -> string -> string
  val app : (wchar -> unit) -> string -> unit
  val fold : ((wchar * 'a) -> 'a) -> 'a -> string -> 'a
  val all : (wchar -> bool) -> string -> bool
  val exists : (wchar -> bool) -> string -> bool
end

structure UTF8 :> UTF8 = struct end
