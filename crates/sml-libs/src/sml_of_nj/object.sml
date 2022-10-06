signature UNSAFE_OBJECT = sig
  type object
  datatype representation = Unboxed | Real | Pair | Record | PolyArray | ByteVector | ByteArray | RealArray | Susp | WeakPtr
  val toObject : 'a -> object
  val boxed : object -> bool
  val unboxed : object -> bool
  val rep : object -> representation
  exception Representation
  val toTuple : object -> object vector
  val toString : object -> string
  val toRef : object -> object ref
  val toArray : object -> object array
  val toExn : object -> exn
  val toReal : object -> real
  val toInt : object -> int
  val toInt32 : object -> Int32.int
  val toWord : object -> Word.word
  val toWord8 : object -> Word8.word
  val toWord32 : object -> Word32.word
end
