signature BOOL = sig
  datatype bool = datatype bool
  val not : bool -> bool
  val toString : bool -> string
  val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
  val fromString : string -> bool option
end

structure Bool :> BOOL = struct end
