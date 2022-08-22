(*!
 * The MONO_ARRAY signature is a generic interface to monomorphic arrays, mutable sequences with
 * constant-time access and update. Monomorphic arrays allow more compact representations than the
 * analogous polymorphic arrays over the same element type.
 *
 * Arrays have a special equality property: two arrays are equal if they are the same array, i.e.,
 * created by the same call to a primitive array constructor such as array, fromList, etc.;
 * otherwise they are not equal. This also holds for arrays of zero length.
 *)
signature MONO_ARRAY = sig
  eqtype array
  type elem
  (*!
   * The corresponding monomorphic vector type. We denote the length of a vector vec of type vector
   * by |vec|.
   *)
  type vector
  (*!
   * The maximum length of arrays supported by this implementation. Attempts to create larger arrays
   * will result in the Size exception being raised.
   *)
  val maxLen : int
  (*!
   * array (n, init) creates a new array of length n; each element is initialized to the value init.
   * If n < 0 or maxLen < n, then the Size exception is raised.
   *)
  val array : int * elem -> array
  (*!
   * fromList l creates a new array from l, whose length is length l and with the i(th) element of l
   * used as the i(th) element of the array. If the length of the list is greater than maxLen, then
   * the Size exception is raised.
   *)
  val fromList : elem list -> array
  (*!
   * tabulate (n, f) creates an array of n elements, where the elements are defined in order of
   * increasing index by applying f to the element's index. This is equivalent to the expression:
   * fromList (List.tabulate (n, f)) If n < 0 or maxLen < n, then the Size exception is raised.
   *)
  val tabulate : int * (int -> elem) -> array
  (*!
   * length arr returns |arr|, the number of elements in the array arr.
   *)
  val length : array -> int
  (*!
   * sub (arr, i) returns the i(th) element of the array arr. If i < 0 or |arr| <= i, then the
   * Subscript exception is raised.
   *)
  val sub : array * int -> elem
  (*!
   * update (arr, i, x) sets the i(th) element of the array arr to x. If i < 0 or |arr| <= i, then
   * the Subscript exception is raised.
   *)
  val update : array * int * elem -> unit
  (*!
   * vector arr generates a vector from arr. Specifically, if vec is the resulting vector, we have
   * |vec| = |arr| and, for 0 <= i < |arr|, element i of vec is sub (arr, i).
   *)
  val vector : array -> vector
  (*!
   * These functions copy the entire array or vector src into the array dst, with the i(th) element
   * in src, for 0 <= i < |src|, being copied to position di + i in the destination array. If di < 0
   * or if |dst| < di+|src|, then the Subscript exception is raised. Implementation note: In copy,
   * if dst and src are equal, we must have di = 0 to avoid an exception, and copy is then the
   * identity.
   *)
  val copy : {src : array, dst : array, di : int} -> unit
  (*!
   * See copy.
   *)
  val copyVec : {src : vector, dst : array, di : int} -> unit
  (*!
   * These apply the function f to the elements of an array in left to right order (i.e., increasing
   * indices). The more general appi function supplies both the element and the element's index to
   * the function f. The expression app f arr is equivalent to: appi (f o #2) arr
   *)
  val appi : (int * elem -> unit) -> array -> unit
  (*!
   * See appi.
   *)
  val app : (elem -> unit) -> array -> unit
  (*!
   * These apply the function f to the elements of an array in left to right order (i.e., increasing
   * indices), and replace each element with the result of applying f. The more general modifyi
   * function supplies both the element and the element's index to the function f. The expression
   * modify f arr is equivalent to: modifyi (f o #2) arr
   *)
  val modifyi : (int * elem -> elem) -> array -> unit
  (*!
   * See modifyi.
   *)
  val modify : (elem -> elem) -> array -> unit
  (*!
   * These fold the function f over all the elements of an array, using the value init as the
   * initial value. The functions foldli and foldl apply the function f from left to right
   * (increasing indices), while the functions foldri and foldr work from right to left (decreasing
   * indices). The more general functions foldli and foldri supply f with the array index of the
   * corresponding element. The indexed versions could be implemented as: fun foldli f init seq =
   * let val len = length seq fun loop (i, b) = if i = len then b else loop(i+1,f(i,sub(seq,i),b))
   * in loop(0,init) end fun foldri f init seq = let val len = length seq fun loop (i, b) = if i =
   * ~1 then b else loop(i-1,f(i,sub(seq,i),b)) in loop(len-1,init) end The expression foldl f init
   * arr is equivalent to: foldli (fn (_, a, x) => f(a, x)) init arr The analogous equivalences hold
   * for foldri and foldr.
   *)
  val foldli : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
  (*!
   * See foldli.
   *)
  val foldri : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
  (*!
   * See foldli.
   *)
  val foldl : (elem * 'b -> 'b) -> 'b -> array -> 'b
  (*!
   * See foldli.
   *)
  val foldr : (elem * 'b -> 'b) -> 'b -> array -> 'b
  (*!
   * These apply f to each element of the array arr, from left to right (i.e., increasing indices),
   * until a true value is returned. If this occurs, the functions return the element; otherwise,
   * they return NONE. The more general version findi also supplies f with the array index of the
   * element and, upon finding an entry satisfying the predicate, returns that index with the
   * element.
   *)
  val findi : (int * elem -> bool) -> array -> (int * elem) option
  (*!
   * See findi.
   *)
  val find : (elem -> bool) -> array -> elem option
  (*!
   * exists f arr applies f to each element x of the array arr, from left to right (i.e., increasing
   * indices), until f x evaluates to true; it returns true if such an x exists and false otherwise.
   *)
  val exists : (elem -> bool) -> array -> bool
  (*!
   * all f arr applies f to each element x of the array arr, from left to right (i.e., increasing
   * indices), until f x evaluates to false; it returns false if such an x exists and true
   * otherwise. It is equivalent to not(exists (not o f) arr)).
   *)
  val all : (elem -> bool) -> array -> bool
  (*!
   * collate f (a1, a2) performs lexicographic comparison of the two arrays using the given ordering
   * f on elements.
   *)
  val collate : (elem * elem -> order) -> array * array -> order
end

structure Word8Array :> MONO_ARRAY
  where type vector = Word8Vector.vector
  where type elem = Word8.word = struct end

structure CharArray :> MONO_ARRAY
  where type vector = CharVector.vector
  where type elem = char = struct end

structure WideCharArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = WideCharVector.vector
  where type elem = WideChar.char = struct end

structure BoolArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = BoolVector.vector
  where type elem = bool = struct end

structure IntArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = IntVector.vector
  where type elem = int = struct end

structure WordArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = WordVector.vector
  where type elem = word = struct end

structure RealArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = RealVector.vector
  where type elem = real = struct end

structure LargeIntArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = LargeIntVector.vector
  where type elem = LargeInt.int = struct end

structure LargeWordArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = LargeWordVector.vector
  where type elem = LargeWord.word = struct end

structure LargeRealArray :> MONO_ARRAY (* OPTIONAL *)
  where type vector = LargeRealVector.vector
  where type elem = LargeReal.real = struct end
