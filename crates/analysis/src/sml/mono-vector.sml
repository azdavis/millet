(*!
The MONO_VECTOR signature is a generic interface to monomorphic vectors, immutable sequences with
constant-time access. Monomorphic vectors allow more compact representations than the analogous
polymorphic vectors over the same element type.
!*)
signature MONO_VECTOR = sig
  type vector
  type elem
  (*!
  The maximum length of vectors supported by this implementation. Attempts to create larger vectors
  will result in the Size exception being raised.
  !*)
  val maxLen : int
  (*!
  fromList l creates a new vector from l, whose length is length l and with the i(th) element of l
  used as the i(th) element of the vector. If the length of the list is greater than maxLen, then
  the Size exception is raised.
  !*)
  val fromList : elem list -> vector
  (*!
  tabulate (n, f) creates a vector of n elements, where the elements are defined in order of
  increasing index by applying f to the element's index. This is equivalent to the expression:
  fromList (List.tabulate (n, f)) If n < 0 or maxLen < n, then the Size exception is raised.
  !*)
  val tabulate : int * (int -> elem) -> vector
  (*!
  length vec returns |vec|, the length (i.e., the number of elements) of the vector vec.
  !*)
  val length : vector -> int
  (*!
  sub (vec, i) returns the i(th) element of the vector vec. If i < 0 or |vec| <= i, then the
  Subscript exception is raised.
  !*)
  val sub : vector * int -> elem
  (*!
  update (vec, i, x) returns a new vector, identical to vec, except the i(th) element of vec is set
  to x. If i < 0 or |vec| <= i, then the Subscript exception is raised.
  !*)
  val update : vector * int * elem -> vector
  (*!
  concat l returns the vector that is the concatenation of the vectors in the list l. If the total
  length of these vectors exceeds maxLen, then the Size exception is raised.
  !*)
  val concat : vector list -> vector
  (*!
  These apply the function f to the elements of a vector in left to right order (i.e., increasing
  indices). The more general appi function supplies both the element and the element's index to the
  function f. The expression app f vec is equivalent to: appi (f o #2) vec
  !*)
  val appi : (int * elem -> unit) -> vector -> unit
  (*!
  See appi.
  !*)
  val app : (elem -> unit) -> vector -> unit
  (*!
  These functions produce new vectors by mapping the function f from left to right over the argument
  vector. The more general mapi function supplies both the element and the element's index to the
  function f. The expression mapi f vec is equivalent to: fromList (List.map f (foldri (fn (i,a,l)
  => (i,a)::l) [] vec)) The expression map f vec is equivalent to: mapi (f o #2) vec
  !*)
  val mapi : (int * elem -> elem) -> vector -> vector
  (*!
  See mapi.
  !*)
  val map : (elem -> elem) -> vector -> vector
  (*!
  These fold the function f over all the elements of a vector, using the value init as the initial
  value. The functions foldli and foldl apply the function f from left to right (increasing
  indices), while the functions foldri and foldr work from right to left (decreasing indices). The
  more general functions foldli and foldri supply both the element and the element's index to the
  function f. Refer to the MONO_ARRAY manual pages for reference implementations of the indexed
  versions. The expression foldl f is equivalent to: foldli (fn (_, a, x) => f(a, x)) A similar
  relation holds between foldr and foldri.
  !*)
  val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
  (*!
  See foldli.
  !*)
  val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
  (*!
  See foldli.
  !*)
  val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
  (*!
  See foldli.
  !*)
  val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
  (*!
  These apply f to each element of the vector vec, from left to right (i.e., increasing indices),
  until a true value is returned. If this occurs, the functions return the element; otherwise, they
  return NONE. The more general version findi also supplies f with the vector index of the element
  and, upon finding an entry satisfying the predicate, returns that index with the element.
  !*)
  val findi : (int * elem -> bool) -> vector -> (int * elem) option
  (*!
  See findi.
  !*)
  val find : (elem -> bool) -> vector -> elem option
  (*!
  exists f vec applies f to each element x of the vector vec, from left to right (i.e., increasing
  indices), until f x evaluates to true; it returns true if such an x exists and false otherwise.
  !*)
  val exists : (elem -> bool) -> vector -> bool
  (*!
  all f vec applies f to each element x of the vector vec, from left to right (i.e., increasing
  indices), until f x evaluates to false; it returns false if such an x exists and true otherwise.
  It is equivalent to not(exists (not o f ) vec)).
  !*)
  val all : (elem -> bool) -> vector -> bool
  (*!
  collate f (v1, v2) performs lexicographic comparison of the two vectors using the given ordering f
  on elements.
  !*)
  val collate : (elem * elem -> order) -> vector * vector -> order
end

structure Word8Vector :> MONO_VECTOR
  where type elem = Word8.word = struct end

structure CharVector :> MONO_VECTOR
  where type vector = String.string
  where type elem = char = CharVector

structure WideCharVector :> MONO_VECTOR (* OPTIONAL *)
  where type vector = WideString.string
  where type elem = WideChar.char = WideCharVector

structure BoolVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = bool = struct end

structure IntVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = int = struct end

structure WordVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = word = struct end

structure RealVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = real = struct end

structure LargeIntVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = LargeInt.int = struct end

structure LargeWordVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = LargeWord.word = struct end

structure LargeRealVector :> MONO_VECTOR (* OPTIONAL *)
  where type elem = LargeReal.real = struct end
