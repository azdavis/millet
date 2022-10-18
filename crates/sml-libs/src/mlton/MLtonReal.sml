signature MLTON_REAL = sig
  type t
  val fromWord : word -> t
  val fromLargeWord : LargeWord.word -> t
  val toWord : IEEEReal.rounding_mode -> t -> word
  val toLargeWord : IEEEReal.rounding_mode -> t -> LargeWord.word
end
