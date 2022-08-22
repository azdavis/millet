(*!
 * Bytes are 8-bit integers as provided by the Word8 structure, but serve the dual role as elements
 * composing the extended ASCII character set. The Byte structure provides functions for converting
 * values between these two roles.
 *)
signature BYTE = sig
  (*!
   * byteToChar i returns the character whose code is i.
   *)
  val byteToChar : Word8.word -> char
  (*!
   * charToByte c returns an 8-bit word holding the code for the character c.
   *)
  val charToByte : char -> Word8.word
  (*!
   * These functions convert between a vector of character codes and the corresponding string. Note
   * that these functions do not perform end-of-line, or other character, translations. The
   * semantics of these functions can be defined as follows, although one expects actual
   * implementations will be more efficient: fun bytesToString bv = CharVector.tabulate(
   * Word8Vector.length bv, fn i => byteToChar(Word8Vector.sub(bv, i))) fun stringToBytes s =
   * Word8Vector.tabulate( String.size s, fn i => charToByte(String.sub(s, i))) Implementation note:
   * For implementations where the underlying representation of the Word8Vector.vector and string
   * types are the same, these functions should be constant-time operations.
   *)
  val bytesToString : Word8Vector.vector -> string
  val stringToBytes : string -> Word8Vector.vector
  (*!
   * unpackStringVec slice returns the string consisting of characters whose codes are held in the
   * vector slice slice.
   *)
  val unpackStringVec : Word8VectorSlice.slice -> string
  (*!
   * returns the string consisting of characters whose codes are held in the array slice slice.
   *)
  val unpackString : Word8ArraySlice.slice -> string
  (*!
   * packString (arr, i, s) puts the substring s into the array arr starting at offset i. It raises
   * Subscript if i < 0 or size s + i > |arr|.
   *)
  val packString : Word8Array.array * int * substring -> unit
end

structure Byte :> BYTE = struct end
