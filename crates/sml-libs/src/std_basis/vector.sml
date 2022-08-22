(*!
 * The Vector structure defines polymorphic vectors, immutable sequences with constant-time access.
 *)
signature VECTOR = sig
  eqtype 'a vector
  (*!
   * The maximum length of vectors supported by this implementation. Attempts to create larger
   * vectors will result in the Size exception being raised.
   *)
  val maxLen : int
  (*!
   * fromList l creates a new vector from l, whose length is length l and with the i(th) element of
   * l used as the i(th) element of the vector. If the length of the list is greater than maxLen,
   * then the Size exception is raised.
   *)
  val fromList : 'a list -> 'a vector
  (*!
   * tabulate (n, f) creates a vector of n elements, where the elements are defined in order of
   * increasing index by applying f to the element's index. This is equivalent to the expression:
   * fromList (List.tabulate (n, f)) If n < 0 or maxLen < n, then the Size exception is raised.
   *)
  val tabulate : int * (int -> 'a) -> 'a vector
  (*!
   * length vec returns |vec|, the length of the vector vec.
   *)
  val length : 'a vector -> int
  (*!
   * sub (vec, i) returns the i(th) element of the vector vec. If i < 0 or |vec| <= i, then the
   * Subscript exception is raised.
   *)
  val sub : 'a vector * int -> 'a
  (*!
   * update (vec, i, x) returns a new vector, identical to vec, except the i(th) element of vec is
   * set to x. If i < 0 or |vec| <= i, then the Subscript exception is raised.
   *)
  val update : 'a vector * int * 'a -> 'a vector
  (*!
   * concat l returns the vector that is the concatenation of the vectors in the list l. If the
   * total length of these vectors exceeds maxLen, then the Size exception is raised.
   *)
  val concat : 'a vector list -> 'a vector
  (*!
   * These apply the function f to the elements of a vector in left to right order (i.e., in order
   * of increasing indices). The more general appi function supplies both the element and the
   * element's index to the function f. These are respectively equivalent to: List.app f (foldri (fn
   * (i,a,l) => (i,a)::l) [] vec) List.app f (foldr (fn (a,l) => a::l) [] vec)
   *)
  val appi : (int * 'a -> unit) -> 'a vector -> unit
  (*!
   * See appi.
   *)
  val app : ('a -> unit) -> 'a vector -> unit
  (*!
   * These functions produce new vectors by mapping the function f from left to right over the
   * argument vector. The more general form mapi supplies f with the vector index of an element
   * along with the element. These are respectively equivalent to: fromList (List.map f (foldri (fn
   * (i,a,l) => (i,a)::l) [] vec)) fromList (List.map f (foldr (fn (a,l) => a::l) [] vec))
   *)
  val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector
  (*!
   * See mapi.
   *)
  val map : ('a -> 'b) -> 'a vector -> 'b vector
  (*!
   * These fold the function f over all the elements of a vector, using the value init as the
   * initial value. The functions foldli and foldl apply the function f from left to right
   * (increasing indices), while the functions foldri and foldr work from right to left (decreasing
   * indices). The more general functions foldli and foldri supply both the element and the
   * element's index to the function f. Refer to the MONO_ARRAY manual pages for reference
   * implementations of the indexed versions. The last two expressions are respectively equivalent
   * to: foldli (fn (_, a, x) => f(a, x)) init vec foldri (fn (_, a, x) => f(a, x)) init vec
   *)
  val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
  (*!
   * See foldli.
   *)
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
  (*!
   * See foldli.
   *)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
  (*!
   * See foldli.
   *)
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
  (*!
   * These apply f to each element of the vector vec, from left to right (i.e., increasing indices),
   * until a true value is returned. If this occurs, the functions return the element; otherwise,
   * they return NONE. The more general version findi also supplies f with the vector index of the
   * element and, upon finding an entry satisfying the predicate, returns that index with the
   * element.
   *)
  val findi : (int * 'a -> bool) -> 'a vector -> (int * 'a) option
  (*!
   * See findi.
   *)
  val find : ('a -> bool) -> 'a vector -> 'a option
  (*!
   * exists f vec applies f to each element x of the vector vec, from left to right (i.e.,
   * increasing indices), until f(x) evaluates to true; it returns true if such an x exists and
   * false otherwise.
   *)
  val exists : ('a -> bool) -> 'a vector -> bool
  (*!
   * all f vec applies f to each element x of the vector vec, from left to right (i.e., increasing
   * indices), until f(x) evaluates to false; it returns false if such an x exists and true
   * otherwise. It is equivalent to not(exists (not o f ) vec)).
   *)
  val all : ('a -> bool) -> 'a vector -> bool
  (*!
   * collate f (v1, v2) performs lexicographic comparison of the two vectors using the given
   * ordering f on elements.
   *)
  val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order
end

structure Vector :> VECTOR = struct end
datatype vector = datatype Vector.vector
