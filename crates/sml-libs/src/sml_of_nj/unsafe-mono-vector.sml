signature UNSAFE_MONO_VECTOR = sig
  type vector
  type elem
  val sub : (vector * int) -> elem
  val update : (vector * int * elem) -> unit
  val create : int -> vector
end
