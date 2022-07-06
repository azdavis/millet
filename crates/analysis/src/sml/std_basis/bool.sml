(*!
The Bool structure provides some basic operations on boolean values.
!*)
signature BOOL = sig
  datatype bool = datatype bool
  (*!
  not b returns the logical negation of the boolean value b.
  !*)
  val not : bool -> bool
  (*!
  toString b returns the string representation of b, either "true" or "false".
  !*)
  val toString : bool -> string
  (*!
  These scan a character source for a boolean value. The first takes a character stream reader getc
  and a stream strm. Ignoring case and initial whitespace, the sequences "true" and "false" are
  converted to the corresponding boolean values. On successful scanning of a boolean value, scan
  returns SOME(b, rest), where b is the scanned value and rest is the remaining character stream.
  The second form scans a boolean from a string s. It returns SOME(b) for a scanned value b;
  otherwise it returns NONE. The function fromString is equivalent to StringCvt.scanString scan.
  !*)
  val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
  (*!
  See scan.
  !*)
  val fromString : string -> bool option
end

structure Bool :> BOOL = struct end
val not = Bool.not
