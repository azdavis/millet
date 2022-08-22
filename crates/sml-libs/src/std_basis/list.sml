(*!
 * The List structure provides a collection of utility functions for manipulating polymorphic lists,
 * traditionally an important datatype in functional programming.
 *
 * Following the concrete syntax provided by the list :: operator, the head of a list appears
 * leftmost. Thus, a traversal of a list from left to right starts with the head, then recurses on
 * the tail. In addition, as a sequence type, a list has an indexing of its elements, with the head
 * having index 0, the second element having index 1, etc.
 *)
signature LIST = sig
  datatype list = datatype list
  (*!
   * This exception indicates that an empty list was given as an argument to a function requiring a
   * non-empty list.
   *)
  exception Empty
  (*!
   * null l returns true if the list l is empty.
   *)
  val null : 'a list -> bool
  (*!
   * length l returns the number of elements in the list l.
   *)
  val length : 'a list -> int
  (*!
   * l1 @ l2 returns the list that is the concatenation of l1 and l2.
   *)
  val @ : 'a list * 'a list -> 'a list
  (*!
   * hd l returns the first element of l. It raises Empty if l is nil.
   *)
  val hd : 'a list -> 'a
  (*!
   * tl l returns all but the first element of l. It raises Empty if l is nil.
   *)
  val tl : 'a list -> 'a list
  (*!
   * last l returns the last element of l. It raises Empty if l is nil.
   *)
  val last : 'a list -> 'a
  (*!
   * getItem l returns NONE if the list is empty, and SOME(hd l,tl l) otherwise. This function is
   * particularly useful for creating value readers from lists of characters. For example, Int.scan
   * StringCvt.DEC getItem has the type (int,char list) StringCvt.reader and can be used to scan
   * decimal integers from lists of characters.
   *)
  val getItem : 'a list -> ('a * 'a list) option
  (*!
   * nth (l, i) returns the i(th) element of the list l, counting from 0. It raises Subscript if i <
   * 0 or i >= length l. We have nth(l,0) = hd l, ignoring exceptions.
   *)
  val nth : 'a list * int -> 'a
  (*!
   * take (l, i) returns the first i elements of the list l. It raises Subscript if i < 0 or i >
   * length l. We have take(l, length l) = l.
   *)
  val take : 'a list * int -> 'a list
  (*!
   * drop (l, i) returns what is left after dropping the first i elements of the list l. It raises
   * Subscript if i < 0 or i > length l. It holds that take(l, i) @ drop(l, i) = l when 0 <= i <=
   * length l. We also have drop(l, length l) = [].
   *)
  val drop : 'a list * int -> 'a list
  (*!
   * rev l returns a list consisting of l's elements in reverse order.
   *)
  val rev : 'a list -> 'a list
  (*!
   * concat l returns the list that is the concatenation of all the lists in l in order.
   * concat[l1,l2,...ln] = l1 @ l2 @ ... @ ln
   *)
  val concat : 'a list list -> 'a list
  (*!
   * revAppend (l1, l2) returns (rev l1) @ l2.
   *)
  val revAppend : 'a list * 'a list -> 'a list
  (*!
   * app f l applies f to the elements of l, from left to right.
   *)
  val app : ('a -> unit) -> 'a list -> unit
  (*!
   * map f l applies f to each element of l from left to right, returning the list of results.
   *)
  val map : ('a -> 'b) -> 'a list -> 'b list
  (*!
   * mapPartial f l applies f to each element of l from left to right, returning a list of results,
   * with SOME stripped, where f was defined. f is not defined for an element of l if f applied to
   * the element returns NONE. The above expression is equivalent to: ((map valOf) o (filter isSome)
   * o (map f)) l
   *)
  val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
  (*!
   * find f l applies f to each element x of the list l, from left to right, until f x evaluates to
   * true. It returns SOME(x) if such an x exists; otherwise it returns NONE.
   *)
  val find : ('a -> bool) -> 'a list -> 'a option
  (*!
   * filter f l applies f to each element x of l, from left to right, and returns the list of those
   * x for which f x evaluated to true, in the same order as they occurred in the argument list.
   *)
  val filter : ('a -> bool) -> 'a list -> 'a list
  (*!
   * partition f l applies f to each element x of l, from left to right, and returns a pair (pos,
   * neg) where pos is the list of those x for which f x evaluated to true, and neg is the list of
   * those for which f x evaluated to false. The elements of pos and neg retain the same relative
   * order they possessed in l.
   *)
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  (*!
   * foldl f init [x1, x2, ..., xn] returns f(xn,...,f(x2, f(x1, init))...) or init if the list is
   * empty.
   *)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
  (*!
   * foldr f init [x1, x2, ..., xn] returns f(x1, f(x2, ..., f(xn, init)...)) or init if the list is
   * empty.
   *)
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
  (*!
   * exists f l applies f to each element x of the list l, from left to right, until f x evaluates
   * to true; it returns true if such an x exists and false otherwise.
   *)
  val exists : ('a -> bool) -> 'a list -> bool
  (*!
   * all f l applies f to each element x of the list l, from left to right, until f x evaluates to
   * false; it returns false if such an x exists and true otherwise. It is equivalent to not(exists
   * (not o f) l)).
   *)
  val all : ('a -> bool) -> 'a list -> bool
  (*!
   * tabulate (n, f) returns a list of length n equal to [f(0), f(1), ..., f(n-1)], created from
   * left to right. It raises Size if n < 0.
   *)
  val tabulate : int * (int -> 'a) -> 'a list
  (*!
   * collate f (l1, l2) performs lexicographic comparison of the two lists using the given ordering
   * f on the list elements.
   *)
  val collate : ('a * 'a -> order) -> 'a list * 'a list -> order
end

structure List :> LIST = struct end
exception Empty = List.Empty
val op @ = List.@
val app = List.app
val concat = List.concat
val foldl = List.foldl
val foldr = List.foldr
val hd = List.hd
val length = List.length
val map = List.map
val null = List.null
val rev = List.rev
val tl = List.tl
