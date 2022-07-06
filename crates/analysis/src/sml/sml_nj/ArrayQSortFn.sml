signature MONO_ARRAY_SORT = sig
  structure A : MONO_ARRAY
  val sort : (A.elem * A.elem -> order) -> A.array -> unit
  val sorted : (A.elem * A.elem -> order) -> A.array -> bool
end

functor ArrayQSortFn (A : MONO_ARRAY) : MONO_ARRAY_SORT = struct end
