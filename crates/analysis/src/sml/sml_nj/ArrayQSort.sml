signature ARRAY_SORT = sig
  val sort : ('a * 'a -> order) -> 'a array -> unit
  val sorted : ('a * 'a -> order) -> 'a array -> bool
end

structure ArrayQSort : ARRAY_SORT = struct end
