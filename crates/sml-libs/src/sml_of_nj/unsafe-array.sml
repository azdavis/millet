signature UNSAFE_ARRAY = sig
  val sub : ('a array * int) -> 'a
  val update : ('a array * int * 'a) -> unit
  val create : (int * 'a) -> 'a array
end
