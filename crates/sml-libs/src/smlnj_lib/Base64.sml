signature BASE64 = sig
  val isBase64 : char -> bool
  val encode : Word8Vector.vector -> string
  val encodeSlice : Word8VectorSlice.slice -> string
  exception Incomplete
  exception Invalid of (int * char)
  val decode : string -> Word8Vector.vector
  val decodeSlice : substring -> Word8Vector.vector
  val decodeStrict : string -> Word8Vector.vector
  val decodeSliceStrict : substring -> Word8Vector.vector
end

structure Base64 : BASE64 = struct end
