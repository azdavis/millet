signature UNSAFE_OBJECT = sig
  (*!
  An abstract type for SML values, from which their representation may be interrogated.
  !*)
  type object
  datatype representation = Unboxed | Real | Pair | Record | PolyArray | ByteVector | ByteArray | RealArray | Susp | WeakPtr
  (*!
  toObject a View any value as an object.
  !*)
  val toObject : 'a -> object
  (*!
  boxed ob Does ob have a boxed (pointer) representation?
  !*)
  val boxed : object -> bool
  (*!
  unboxed ob Does ob have an unboxed (tagged, nonpointer) representation?
  !*)
  val unboxed : object -> bool
  (*!
  rep ob What is the representation of ob?
  !*)
  val rep : object -> representation
  exception Representation
  (*!
  toTuple ob If ob is a record, tuple, vector, or real-array, get a vector of its fields; otherwise
  raise Representation.
  !*)
  val toTuple : object -> object vector
  (*!
  toString ob If ob is really a string, get its string value; otherwise raise Representation.
  !*)
  val toString : object -> string
  (*!
  toRef ob If ob is really a ref or an array of length 1, view it as a ref, otherwise raise
  Representation.
  !*)
  val toRef : object -> object ref
  (*!
  toArray ob If ob is really an array, view it as a array, otherwise raise Representation.
  !*)
  val toArray : object -> object array
  (*!
  toExn ob If ob is really an exception value, view it as a exn, otherwise raise Representation.
  !*)
  val toExn : object -> exn
  (*!
  toReal ob If ob is really an floating-point number, view it as a real, otherwise raise
  Representation.
  !*)
  val toReal : object -> real
  (*!
  toInt ob If ob is really a tagged 31-bit integer, view it as an int, otherwise raise
  Representation.
  !*)
  val toInt : object -> int
  (*!
  toInt32 ob If ob is really a 32-bit integer or byte-vector of length 4, view it as an Int32.int,
  otherwise raise Representation.
  !*)
  val toInt32 : object -> Int32.int
  (*!
  toWord ob If ob is a tagged 31-bit value, view it as a word, otherwise raise Representation.
  !*)
  val toWord : object -> Word.word
  (*!
  toWord8 ob If ob is a tagged 31-bit value, view it as a Word8.word, otherwise raise
  Representation.
  !*)
  val toWord8 : object -> Word8.word
  (*!
  toWord32 ob If ob is really a 32-bit value or byte-vector of length 4, view it as a Word32.word,
  otherwise raise Representation.
  !*)
  val toWord32 : object -> Word32.word
end
