signature MONO_DYNAMIC_ARRAY = sig
  type elem
  type array
  val array : (int * elem) -> array
  val subArray : array * int * int -> array
  val fromList : elem list * elem -> array
  val toList : array -> elem list
  val tabulate: int * (int -> elem) * elem -> array
  val default : array -> elem
  val sub : array * int -> elem
  val update : array * int * elem -> unit
  val bound : array -> int
  val truncate : array * int -> unit
end

functor DynamicArrayFn (A : MONO_ARRAY) : MONO_DYNAMIC_ARRAY = struct end
