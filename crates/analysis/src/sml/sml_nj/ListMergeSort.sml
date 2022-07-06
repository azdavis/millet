signature LIST_SORT = sig
  val sort : ('a * 'a -> bool) -> 'a list -> 'a list
  val uniqueSort : ('a * 'a -> order) -> 'a list -> 'a list
  val sorted : ('a * 'a -> bool) -> 'a list -> bool
end

structure ListMergeSort : LIST_SORT = struct end
