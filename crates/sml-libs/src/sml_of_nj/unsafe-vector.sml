signature UNSAFE_VECTOR = sig
  val sub : ('a vector * int) -> 'a
  val create : (int * 'a list) -> 'a vector
end
