structure FNVHash : sig
  val offsetBasis : Word64.word
  val hashByte : Word8.word * Word64.word -> Word64.word
  val hashChar : char * Word64.word -> Word64.word
  val hashString : string -> word
  val hashSubstring : substring -> word
end = struct end
