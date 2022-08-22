(*!
 * The ArraySlice structure provides an abstraction of subarrays for polymorphic arrays. A slice
 * value can be viewed as a triple (a, i, n), where a is the underlying array, i is the starting
 * index, and n is the length of the subarray, with the constraint that 0 <= i <= i + n <= |a|.
 * Slices provide a convenient notation for specifying and operating on a contiguous subset of
 * elements in an array.
 *)
signature ARRAY_SLICE = sig
  type 'a slice
  (*!
   * length sl returns |sl|, the length (i.e., number of elements) of the slice. This is equivalent
   * to #3 (base sl).
   *)
  val length : 'a slice -> int
  (*!
   * sub (sl, i) returns the i(th) element of the slice sl. If i < 0 or |sl| <= i, then the
   * Subscript exception is raised.
   *)
  val sub : 'a slice * int -> 'a
  (*!
   * update (sl, i, a) sets the i(th) element of the slice sl to a. If i < 0 or |sl| <= i, then the
   * Subscript exception is raised.
   *)
  val update : 'a slice * int * 'a -> unit
  (*!
   * full arr creates a slice representing the entire array arr. It is equivalent to slice(arr, 0,
   * NONE)
   *)
  val full : 'a Array.array -> 'a slice
  (*!
   * slice (arr, i, sz) creates a slice based on the array arr starting at index i of the array. If
   * sz is NONE, the slice includes all of the elements to the end of the array, i.e.,
   * arr[i..|arr|-1]. This raises Subscript if i < 0 or |arr| < i. If sz is SOME(j), the slice has
   * length j, that is, it corresponds to arr[i..i+j-1]. It raises Subscript if i < 0 or j < 0 or
   * |arr| < i + j. Note that, if defined, slice returns an empty slice when i = |arr|.
   *)
  val slice : 'a Array.array * int * int option -> 'a slice
  (*!
   * subslice (sl, i, sz) creates a slice based on the given slice sl starting at index i of sl. If
   * sz is NONE, the slice includes all of the elements to the end of the slice, i.e.,
   * sl[i..|sl|-1]. This raises Subscript if i < 0 or |sl| < i. If sz is SOME(j), the slice has
   * length j, that is, it corresponds to sl[i..i+j-1]. It raises Subscript if i < 0 or j < 0 or
   * |sl| < i + j. Note that, if defined, subslice returns an empty slice when i = |sl|.
   *)
  val subslice : 'a slice * int * int option -> 'a slice
  (*!
   * base sl returns a triple (arr, i, n) representing the concrete representation of the slice. arr
   * is the underlying array, i is the starting index, and n is the length of the slice.
   *)
  val base : 'a slice -> 'a Array.array * int * int
  (*!
   * vector sl generates a vector from the slice sl. Specifically, the result is equivalent to
   * Vector.tabulate (length sl, fn i => sub (sl, i))
   *)
  val vector : 'a slice -> 'a Vector.vector
  (*!
   * These functions copy the given slice into the array dst, with the i(th) element of src, for 0
   * <= i < |src|, being copied to position di + i in the destination array. If di < 0 or if |dst| <
   * di+|src|, then the Subscript exception is raised. Implementation note: The copy function must
   * correctly handle the case in which dst and the base array of src are equal, and the source and
   * destination slices overlap.
   *)
  val copy : { src : 'a slice, dst : 'a Array.array, di : int } -> unit
  (*!
   * See copy.
   *)
  val copyVec : { src : 'a VectorSlice.slice, dst : 'a Array.array, di : int } -> unit
  (*!
   * isEmpty sl returns true if sl has length 0.
   *)
  val isEmpty : 'a slice -> bool
  (*!
   * getItem sl returns the first item in sl and the rest of the slice, or NONE if sl is empty.
   *)
  val getItem : 'a slice -> ('a * 'a slice) option
  (*!
   * These functions apply the function f to the elements of a slice in order of increasing indices.
   * The more general appi function supplies f with the index of the corresponding element in the
   * slice. The expression app f sl is equivalent to appi (f o #2) sl.
   *)
  val appi : (int * 'a -> unit) -> 'a slice -> unit
  (*!
   * See appi.
   *)
  val app : ('a -> unit) -> 'a slice -> unit
  (*!
   * These functions apply the function f to the elements of a slice in order of increasing indices,
   * and replace each element with the result. The more general modifyi supplies f with the index of
   * the corresponding element in the slice. The expression modify f sl is equivalent to modifyi (f
   * o #2) sl.
   *)
  val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
  (*!
   * See modifyi.
   *)
  val modify : ('a -> 'a) -> 'a slice -> unit
  (*!
   * These functions fold the function f over the elements of a slice, using the value init as the
   * initial value. The functions foldli and foldl apply the function f from left to right
   * (increasing indices), while the functions foldri and foldr work from right to left (decreasing
   * indices). The more general functions foldli and foldri supply f with the index of the
   * corresponding element in the slice. Refer to the MONO_ARRAY manual pages for reference
   * implementations of the indexed versions. The expression foldl f init sl is equivalent to:
   * foldli (fn (_, a, x) => f(a, x)) init sl The analogous equivalence holds for foldri and foldr.
   *)
  val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
   * See foldli.
   *)
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
   * See foldli.
   *)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
   * See foldli.
   *)
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  (*!
   * These functions apply f to each element of the slice sl, in order of increasing indices, until
   * a true value is returned. If this occurs, the functions return the element; otherwise, they
   * return NONE. The more general version findi also supplies f with the index of the element in
   * the slice and, upon finding an entry satisfying the predicate, returns that index with the
   * element.
   *)
  val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  (*!
   * See findi.
   *)
  val find : ('a -> bool) -> 'a slice -> 'a option
  (*!
   * exists f sl applies f to each element x of the slice sl, in order of increasing indices, until
   * f x evaluates to true; it returns true if such an x exists and false otherwise.
   *)
  val exists : ('a -> bool) -> 'a slice -> bool
  (*!
   * all f sl applies f to each element x of the slice sl, from left to right (i.e., increasing
   * indices), until f x evaluates to false; it returns false if such an x exists and true
   * otherwise. It is equivalent to not(exists (not o f) l)).
   *)
  val all : ('a -> bool) -> 'a slice -> bool
  (*!
   * collate f (sl, sl2) performs lexicographic comparison of the two slices using the given
   * ordering f on elements.
   *)
  val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order
end

structure ArraySlice :> ARRAY_SLICE = struct end
