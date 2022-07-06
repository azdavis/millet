functor BSearchFn (A : MONO_ARRAY) : sig
  structure A : MONO_ARRAY
  val bsearch : (('a * A.elem) -> order) -> ('a * A.array) -> (int * A.elem) option
end = struct end
