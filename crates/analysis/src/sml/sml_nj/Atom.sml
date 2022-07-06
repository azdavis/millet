signature ATOM = sig
  type atom
  val atom : string -> atom
  val atom' : substring -> atom
  val toString : atom -> string
  val same : (atom * atom) -> bool
  val sameAtom : (atom * atom) -> bool
  val compare : (atom * atom) -> order
  val lexCompare : (atom * atom) -> order
  val hash : atom -> word
end

structure Atom : ATOM = struct end
