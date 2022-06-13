structure String :> sig type string end = struct end
structure CharVector :> sig type vector end = struct end
structure Char :> sig type char end = struct end
structure WideCharVector :> sig type vector end = struct end
structure WideChar :> sig type char end = struct end

signature STRING = sig
  eqtype string
  eqtype char
  val maxSize : int
  val size : string -> int
  val sub : string * int -> char
  val extract : string * int * int option -> string
  val substring : string * int * int -> string
  val ^ : string * string -> string
  val concat : string list -> string
  val concatWith : string -> string list -> string
  val str : char -> string
  val implode : char list -> string
  val explode : string -> char list
  val map : (char -> char) -> string -> string
  val translate : (char -> string) -> string -> string
  val tokens : (char -> bool) -> string -> string list
  val fields : (char -> bool) -> string -> string list
  val isPrefix : string -> string -> bool
  val isSubstring : string -> string -> bool
  val isSuffix : string -> string -> bool
  val compare : string * string -> order
  val collate : (char * char -> order) -> string * string -> order
  val < : string * string -> bool
  val <= : string * string -> bool
  val > : string * string -> bool
  val >= : string * string -> bool
  val toString : string -> String.string
  val scan : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
  val fromString : String.string -> string option
  val toCString : string -> String.string
  val fromCString : String.string -> string option
end

structure String :> STRING
  where type string = string
  where type string = CharVector.vector
  where type char = Char.char
  = String

structure WideString :> STRING (* OPTIONAL *)
  where type string = WideCharVector.vector
  where type char = WideChar.char
  = struct end
