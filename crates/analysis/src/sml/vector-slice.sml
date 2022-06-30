(*!
The VectorSlice structure provides an abstraction of subvectors for polymorphic vectors. A slice
value can be viewed as a triple (v, i, n), where v is the underlying vector, i is the starting
index, and n is the length of the subvector, with the constraint that 0 <= i <= i + n <= |v|, where
|v| is the length of v. Slices provide a convenient notation for specifying and operating on a
contiguous subset of elements in a vector.
!*)
signature VECTOR_SLICE = sig
  type 'a slice
  (*!
  length sl returns |sl|, the length (i.e., number of elements) of the slice.
  !*)
  val length : 'a slice -> int
  (*!
  sub (sl, i) returns the i(th) element of the slice sl. If i < 0 or |sl| <= i, then the Subscript
  exception is raised.
  !*)
  val sub : 'a slice * int -> 'a
  (*!
  full vec creates a slice representing the entire vector vec. It is equivalent to slice(vec, 0,
  NONE)
  !*)
  val full : 'a Vector.vector -> 'a slice
  (*!
  slice (vec, i, sz) creates a slice based on the vector vec starting at index i of the vector vec.
  If sz is NONE, the slice includes all of the elements to the end of the vector, i.e.,
  vec[i..|vec|-1]. This raises Subscript if i < 0 or |vec| < i. If sz is SOME(j), the slice has
  length j, that is, it corresponds to vec[i..i+j-1]. It raises Subscript if i < 0 or j < 0 or |arr|
  < i + j. Note that, if defined, slice returns an empty slice when i = |vec|.
  !*)
  val slice : 'a Vector.vector * int * int option -> 'a slice
  (*!
  subslice (sl, i, sz) creates a slice based on the given slice sl starting at index i of sl. If sz
  is NONE, the slice includes all of the elements to the end of the slice, i.e., sl[i..|sl|-1]. This
  raises Subscript if i < 0 or |sl| < i. If sz is SOME(j), the slice has length j, that is, it
  corresponds to sl[i..i+j-1]. It raises Subscript if i < 0 or j < 0 or |sl| < i + j. Note that, if
  defined, slice returns an empty slice when i = |sl|.
  !*)
  val subslice : 'a slice * int * int option -> 'a slice
  (*!
  base sl returns a triple (vec, i, n) representing the concrete representation of the slice. vec is
  the underlying vector, i is the starting index, and n is the length of the slice.
  !*)
  val base : 'a slice -> 'a Vector.vector * int * int
  (*!
  vector sl generates a vector from the slice sl. Specifically, the result is equivalent to
  Vector.tabulate (length sl, fn i => sub (sl, i))
  !*)
  val vector : 'a slice -> 'a Vector.vector
  (*!
  concat l is the concatenation of all the slices in l. This raises Size if the sum of all the
  lengths is greater than Vector.maxLen.
  !*)
  val concat : 'a slice list -> 'a Vector.vector
  (*!
  isEmpty sl returns true if sl has length 0.
  !*)
  val isEmpty : 'a slice -> bool
  (*!
  getItem sl returns the first item in sl and the rest of the slice, or NONE if sl is empty.
  !*)
  val getItem : 'a slice -> ('a * 'a slice) option
  (*!
  These apply the function f to the elements of a slice in left to right order (i.e., increasing
  indices). The more general appi function supplies f with the index of the corresponding element in
  the slice. The expression app f sl is equivalent to appi (f o #2) sl.
  !*)
  val appi : (int * 'a -> unit) -> 'a slice -> unit
  (*!
  See appi.
  !*)
  val app : ('a -> unit) -> 'a slice -> unit
  (*!
  These functions generate new vectors by mapping the function f from left to right over the
  argument slice. The more general mapi function supplies both the element and the element's index
  in the slice to the function f. The first expression is equivalent to: let fun ff (i,a,l) =
  f(i,a)::l in Vector.fromList (rev (foldli ff [] sl)) end The latter expression is equivalent to:
  mapi (f o #2) sl
  !*)
  val mapi : (int * 'a -> 'b) -> 'a slice -> 'b Vector.vector
  (*!
  See mapi.
  !*)
  val map : ('a -> 'b) -> 'a slice -> 'b Vector.vector
  (*!
  These fold the function f over all the elements of a vector slice, using the value init as the
  initial value. The functions foldli and foldl apply the function f from left to right (increasing
  indices), while the functions foldri and foldr work from right to left (decreasing indices). The
  more general functions foldli and foldri supply f with the index of the corresponding element in
  the slice. Refer to the MONO_ARRAY manual pages for reference implementations of the indexed
  versions. The expression foldl f init sl is equivalent to: foldli (fn (_, a, x) => f(a, x)) init
  sl The analogous equivalence holds for foldri and foldr.
  !*)
  val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
  See foldli.
  !*)
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
  See foldli.
  !*)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
  See foldli.
  !*)
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
  These apply f to each element of the slice sl, from left to right (i.e., increasing indices),
  until a true value is returned. If this occurs, the functions return the element; otherwise, they
  return NONE. The more general version findi also supplies f with the index of the element in the
  slice and, upon finding an entry satisfying the predicate, returns that index with the element.
  !*)
  val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  (*!
  See findi.
  !*)
  val find : ('a -> bool) -> 'a slice -> 'a option
  (*!
  exists f sl applies f to each element x of the slice sl, from left to right (i.e., increasing
  indices), until f(x) evaluates to true; it returns true if such an x exists and false otherwise.
  !*)
  val exists : ('a -> bool) -> 'a slice -> bool
  (*!
  all f sl applies f to each element x of the slice sl, from left to right (i.e., increasing
  indices), until f(x) evaluates to false; it returns false if such an x exists and true otherwise.
  It is equivalent to not(exists (not o f ) sl)).
  !*)
  val all : ('a -> bool) -> 'a slice -> bool
  (*!
  collate f (sl, sl2) performs lexicographic comparison of the two slices using the given ordering f
  on elements.
  !*)
  val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order
end

structure VectorSlice :> VECTOR_SLICE = struct end
