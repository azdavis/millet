(*!
 * The PACK_REAL signature specifies the interface for packing and unpacking floating-point numbers
 * into Word8 vectors and arrays. This provides a mechanism for transmitting floating-point values
 * over a network.
 *
 * For each optional Real<N> structure provided by an implementation, the implementation may also
 * provide a pair of structures PackReal<N>Big and PackReal<N>Little. These structures share the
 * real type defined in Real<N>. The PackReal<N>Big structures perform big-endian packing and
 * unpacking, and the PackReal<N>Little structures perform little-endian packing and unpacking.
 *
 * In addition, an implementation may provide the structures PackRealBig and PackRealLittle, which
 * are aliases for the PACK_REAL structures related to the default Real structure.
 *)
signature PACK_REAL (* OPTIONAL *) = sig
  type real
  (*!
   * The number of bytes per element, sufficient to store a value of type real.
   *)
  val bytesPerElem : int
  (*!
   * isBigEndian is true if the structure implements a big-endian view of the data.
   *)
  val isBigEndian : bool
  (*!
   * These functions pack and unpack floating-point values into and out of Word8Vector.vector
   * values. The function fromBytes raises the Subscript exception if the argument vector does not
   * have length at least bytesPerElem; otherwise the first bytesPerElem bytes are used.
   *)
  val toBytes : real -> Word8Vector.vector
  val fromBytes : Word8Vector.vector -> real
  (*!
   * These functions extract the subsequence seq[bytesPerElem*i..bytesPerElem*(i+1)-1] of the
   * aggregate seq and convert it into a real value according to the endianness of the structure.
   * They raise the Subscript exception if i < 0 or if Word8Array.length seq < bytesPerElem * (i +
   * 1).
   *)
  val subVec : Word8Vector.vector * int -> real
  (*!
   * See subVec.
   *)
  val subArr : Word8Array.array * int -> real
  (*!
   * update (arr, i, r) stores r into the bytes bytesPerElem*i through bytesPerElem*(i+1)-1 of the
   * array arr, according to the structure's endianness. It raises the Subscript exception if i < 0
   * or if Word8Array.length arr < bytesPerElem * (i + 1).
   *)
  val update : Word8Array.array * int * real -> unit
end

structure PackRealBig :> PACK_REAL (* OPTIONAL *)
  where type real = Real.real = struct end
structure PackRealLittle :> PACK_REAL (* OPTIONAL *)
  where type real = Real.real = struct end
structure PackReal64Big :> PACK_REAL (* OPTIONAL *)
  where type real = Real64.real = struct end
structure PackReal64Little :> PACK_REAL (* OPTIONAL *)
  where type real = Real64.real = struct end
