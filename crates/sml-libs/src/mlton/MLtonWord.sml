signature MLTON_WORD = sig
  type t
  val bswap : t -> t
  val rol : t * word -> t
  val ror : t * word -> t
end
