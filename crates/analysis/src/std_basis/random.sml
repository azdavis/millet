(* https://www.smlnj.org/doc/smlnj-lib/Util/str-Random.html *)
signature RANDOM = sig
  type rand
  val rand : (int * int) -> rand
  val toString : rand -> string
  val fromString : string -> rand
  val randInt : rand -> int
  val randNat : rand -> int
  val randReal : rand -> real
  val randRange : (int * int) -> rand -> int
end

structure Random :> RANDOM = struct end
