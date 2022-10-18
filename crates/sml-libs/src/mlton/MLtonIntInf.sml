signature MLTON_INT_INF = sig
  type t = IntInf.int
  val areSmall : t * t -> bool
  val gcd : t * t -> t
  val isSmall : t -> bool
  structure BigWord : WORD
  structure SmallInt : INTEGER
  datatype rep = Big of BigWord.word vector | Small of SmallInt.int
  val rep : t -> rep
  val fromRep : rep -> t option
end
