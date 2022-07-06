signature LIST_FORMAT = sig
  val fmt : { init : string, sep : string, final : string, fmt : 'a -> string } -> 'a list -> string
  val listToString : ('a -> string) -> 'a list -> string
  val scan : { init : string, sep : string, final : string, scan : (char, 'b) StringCvt.reader -> ('a, 'b) StringCvt.reader } -> (char, 'b) StringCvt.reader -> ('a list, 'b) StringCvt.reader
end

structure ListFormat : LIST_FORMAT = struct end
