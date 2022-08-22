(*!
 * The MONO_VECTOR_SLICE signature provides an abstraction of subarrays for monomorphic immutable
 * arrays or vectors. A slice value can be viewed as a triple (v, i, n), where v is the underlying
 * vector, i is the starting index, and n is the length of the subarray, with the constraint that 0
 * <= i <= i + n <= |v|, where |v| is the length of the vector v. Slices provide a convenient
 * notation for specifying and operating on a contiguous subset of elements in a vector.
 *)
signature MONO_VECTOR_SLICE = sig
  type elem
  (*!
   * The underlying monomorphic vector type. We denote the length of a vector vec of type vector by
   * |vec|.
   *)
  type vector
  type slice
  (*!
   * length sl returns |sl|, the length (i.e., number of elements) of the slice.
   *)
  val length : slice -> int
  (*!
   * sub (sl, i) returns the i(th) element of the slice sl. If i < 0 or |sl| <= i, then the
   * Subscript exception is raised.
   *)
  val sub : slice * int -> elem
  (*!
   * full vec creates a slice representing the entire vector vec. It is equivalent to slice(vec, 0,
   * NONE)
   *)
  val full : vector -> slice
  (*!
   * slice (vec, i, sz) creates a slice based on the vector vec starting at index i of the vector
   * vec. If sz is NONE, the slice includes all of the elements to the end of the vector, i.e.,
   * vec[i..|vec|-1]. This raises Subscript if i < 0 or |vec| < i. If sz is SOME(j), the slice has
   * length j, that is, it corresponds to vec[i..i+j-1]. It raises Subscript if i < 0 or j < 0 or
   * |vec| < i + j. Note that, if defined, slice returns an empty slice when i = |vec|.
   *)
  val slice : vector * int * int option -> slice
  (*!
   * subslice (sl, i, sz) creates a slice based on the given slice sl starting at index i of sl. If
   * sz is NONE, the slice includes all of the elements to the end of the slice, i.e.,
   * sl[i..|sl|-1]. This raises Subscript if i < 0 or |sl| < i. If sz is SOME(j), the slice has
   * length j, that is, it corresponds to sl[i..i+j-1]. It raises Subscript if i < 0 or j < 0 or
   * |sl| < i + j. Note that, if defined, slice returns an empty slice when i = |sl|.
   *)
  val subslice : slice * int * int option -> slice
  (*!
   * base sl returns a triple (vec, i, n) representing the concrete representation of the slice. vec
   * is the underlying vector, i is the starting index, and n is the length of the slice.
   *)
  val base : slice -> vector * int * int
  (*!
   * vector sl generates a vector from the slice sl. Specifically, if vec is the resulting vector,
   * we have |vec| = |sl| and, for 0 <= i < |sl|, element i of vec is sub (sl, i).
   *)
  val vector : slice -> vector
  (*!
   * concat l is the concatenation of all the vectors in l. This raises Size if the sum of all the
   * lengths is greater than the maximum length allowed by vectors of type vector.
   *)
  val concat : slice list -> vector
  (*!
   * isEmpty sl returns true if sl has length 0.
   *)
  val isEmpty : slice -> bool
  (*!
   * getItem sl returns the first item in sl and the rest of the slice, or NONE if sl is empty.
   *)
  val getItem : slice -> (elem * slice) option
  (*!
   * These apply the function f to the elements of a slice in left to right order (i.e., increasing
   * indices). The more general appi function supplies f with the index of the corresponding element
   * in the slice. The expression app f sl is equivalent to appi (f o #2) sl.
   *)
  val appi : (int * elem -> unit) -> slice -> unit
  (*!
   * See appi.
   *)
  val app : (elem -> unit) -> slice -> unit
  (*!
   * These functions generate new vectors by mapping the function f from left to right over the
   * argument slice. The more general mapi function supplies both the element and the element's
   * index in the slice to the function f. The latter expression is equivalent to: mapi (f o #2) sl
   *)
  val mapi : (int * elem -> elem) -> slice -> vector
  (*!
   * See mapi.
   *)
  val map : (elem -> elem) -> slice -> vector
  (*!
   * These fold the function f over all the elements of a vector slice, using the value init as the
   * initial value. The functions foldli and foldl apply the function f from left to right
   * (increasing indices), while the functions foldri and foldr work from right to left (decreasing
   * indices). The more general functions foldli and foldri supply f with the index of the
   * corresponding element in the slice. Refer to the MONO_ARRAY manual pages for reference
   * implementations of the indexed versions. The expression foldl f init sl is equivalent to:
   * foldli (fn (_, a, x) => f(a, x)) init sl The analogous equivalence holds for foldri and foldr.
   *)
  val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  (*!
   * See foldli.
   *)
  val foldr : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  (*!
   * See foldli.
   *)
  val foldl : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  (*!
   * See foldli.
   *)
  val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  (*!
   * These apply f to each element of the slice sl, from left to right (i.e., increasing indices),
   * until a true value is returned. If this occurs, the functions return the element; otherwise,
   * they return NONE. The more general version findi also supplies f with the index of the element
   * in the slice and, upon finding an entry satisfying the predicate, returns that index with the
   * element.
   *)
  val findi : (int * elem -> bool) -> slice -> (int * elem) option
  (*!
   * See findi.
   *)
  val find : (elem -> bool) -> slice -> elem option
  (*!
   * exists f sl applies f to each element x of the slice sl, from left to right (i.e., increasing
   * indices), until f x evaluates to true; it returns true if such an x exists and false otherwise.
   *)
  val exists : (elem -> bool) -> slice -> bool
  (*!
   * all f sl applies f to each element x of the slice sl, from left to right (i.e., increasing
   * indices), until f x evaluates to false; it returns false if such an x exists and true
   * otherwise. It is equivalent to not(exists (not o f) sl)).
   *)
  val all : (elem -> bool) -> slice -> bool
  (*!
   * collate f (sl, sl2) performs lexicographic comparison of the two slices using the given
   * ordering f on elements.
   *)
  val collate : (elem * elem -> order) -> slice * slice -> order
end

structure Word8VectorSlice :> MONO_VECTOR_SLICE
  where type vector = Word8Vector.vector
  where type elem = Word8.word = struct end

structure CharVectorSlice :> MONO_VECTOR_SLICE
  where type slice = Substring.substring
  where type vector = String.string
  where type elem = char = CharVectorSlice

structure WideCharVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type slice = WideSubstring.substring
  where type vector = WideString.string
  where type elem = WideChar.char = WideCharVectorSlice

structure BoolVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = BoolVector.vector
  where type elem = bool = struct end

structure IntVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = IntVector.vector
  where type elem = int = struct end

structure WordVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = WordVector.vector
  where type elem = word = struct end

structure RealVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = RealVector.vector
  where type elem = real = struct end

structure LargeIntVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = LargeIntVector.vector
  where type elem = LargeInt.int = struct end

structure LargeWordVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = LargeWordVector.vector
  where type elem = LargeWord.word = struct end

structure LargeRealVectorSlice :> MONO_VECTOR_SLICE (* OPTIONAL *)
  where type vector = LargeRealVector.vector
  where type elem = LargeReal.real = struct end
