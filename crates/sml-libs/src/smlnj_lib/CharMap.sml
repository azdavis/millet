signature CHAR_MAP = sig
  type 'a char_map
  val mkCharMap : {default : 'a, bindings : (string * 'a) list} -> 'a char_map
  val mapChr : 'a char_map -> char -> 'a
  val mapStrChr : 'a char_map -> (string * int) -> 'a
end

structure CharMap :> CHAR_MAP = struct end
