structure Rand : sig
  type rand = Word.word
  val randMin : rand
  val randMax : rand
  val random : rand -> rand
  val mkRandom : rand -> unit -> rand
  val norm : rand -> real
  val range : (int * int) -> rand -> int
end = struct end
