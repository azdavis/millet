(*!
The PackWord<N>Big and PackWord<N>Little structures provide facilities for packing and unpacking
N-bit word elements into Word8 vectors. This mechanism allows word values to be transmitted in
binary format over networks. The PackWord<N>Big structures perform big-endian packing and unpacking,
while the PackWord<N>Little structures perform little-endian packing and unpacking.
!*)
signature PACK_WORD (* OPTIONAL *) = sig
  (*!
  The number of bytes per element. Most implementations will provide several structures with values
  of bytesPerElem that are small powers of two (e.g., 1, 2, 4, and 8, corresponding to N of 8, 16,
  32, 64, respectively).
  !*)
  val bytesPerElem : int
  (*!
  True if the structure implements a big-endian view of the data (most-significant byte first).
  Otherwise, the structure implements a little-endian view (least-significant byte first).
  !*)
  val isBigEndian : bool
  (*!
  These extract the subvector vec[bytesPerElem*i..bytesPerElem*(i+1)-1] of the vector vec and
  convert it into a word according to the endianness of the structure. The subVecX version extends
  the sign bit (most significant bit) when converting the subvector to a word. The functions raise
  the Subscript exception if i < 0 or if Word8Vector.length vec < bytesPerElem * (i + 1).
  !*)
  val subVec : Word8Vector.vector * int -> LargeWord.word
  (*!
  See subVec.
  !*)
  val subVecX : Word8Vector.vector * int -> LargeWord.word
  (*!
  These extract the subarray arr[bytesPerElem*i..bytesPerElem*(i+1)-1] of the array arr and convert
  it into a word according to the endianness of the structure. The subArrX version extends the sign
  bit (most significant bit) when converting the subarray into a word. The functions raise the
  Subscript exception if i < 0 or if Word8Array.length arr < bytesPerElem * (i+1).
  !*)
  val subArr : Word8Array.array * int -> LargeWord.word
  (*!
  See subArr.
  !*)
  val subArrX : Word8Array.array * int -> LargeWord.word
  (*!
  update (arr, i, w) stores the bytesPerElem low-order bytes of the word w into the bytes
  bytesPerElem*i through bytesPerElem*(i+1)-1 of the array arr, according to the structure's
  endianness. It raises the Subscript exception if i < 0 or if Word8Array.length arr < bytesPerElem
  * (i+1).
  !*)
  val update : Word8Array.array * int * LargeWord.word -> unit
end

structure PackWord32Big : PACK_WORD = struct end
structure PackWord32Little : PACK_WORD = struct end
structure PackWord64Big : PACK_WORD = struct end
structure PackWord64Little : PACK_WORD = struct end
