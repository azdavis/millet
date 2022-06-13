signature MONO_ARRAY_SLICE = sig
  type elem
  type array
  type slice
  type vector
  type vector_slice
  val length : slice -> int
  val sub : slice * int -> elem
  val update : slice * int * elem -> unit
  val full : array -> slice
  val slice : array * int * int option -> slice
  val subslice : slice * int * int option -> slice
  val base : slice -> array * int * int
  val vector : slice -> vector
  val copy : {src : slice, dst : array, di : int} -> unit
  val copyVec : {src : vector_slice, dst : array, di : int} -> unit
  val isEmpty : slice -> bool
  val getItem : slice -> (elem * slice) option
  val appi : (int * elem -> unit) -> slice -> unit
  val app : (elem -> unit) -> slice -> unit
  val modifyi : (int * elem -> elem) -> slice -> unit
  val modify : (elem -> elem) -> slice -> unit
  val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldr : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldl : (elem * 'b -> 'b) -> 'b -> slice -> 'b
  val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
  val findi : (int * elem -> bool) -> slice -> (int * elem) option
  val find : (elem -> bool) -> slice -> elem option
  val exists : (elem -> bool) -> slice -> bool
  val all : (elem -> bool) -> slice -> bool
  val collate : (elem * elem -> order) -> slice * slice -> order
end

structure Word8ArraySlice :> MONO_ARRAY_SLICE
  where type vector = Word8Vector.vector
  where type vector_slice = Word8VectorSlice.slice
  where type array = Word8Array.array
  where type elem = Word8.word = struct end

structure CharArraySlice :> MONO_ARRAY_SLICE
  where type vector = CharVector.vector
  where type vector_slice = CharVectorSlice.slice
  where type array = CharArray.array
  where type elem = char = struct end

structure WideCharArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = WideCharVector.vector
  where type vector_slice = WideCharVectorSlice.slice
  where type array = WideCharArray.array
  where type elem = WideChar.char = struct end

structure BoolArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = BoolVector.vector
  where type vector_slice = BoolVectorSlice.slice
  where type array = BoolArray.array
  where type elem = bool = struct end

structure IntArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = IntVector.vector
  where type vector_slice = IntVectorSlice.slice
  where type array = IntArray.array
  where type elem = int = struct end

structure WordArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = WordVector.vector
  where type vector_slice = WordVectorSlice.slice
  where type array = WordArray.array
  where type elem = word = struct end

structure RealArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = RealVector.vector
  where type vector_slice = RealVectorSlice.slice
  where type array = RealArray.array
  where type elem = real = struct end

structure LargeIntArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = LargeIntVector.vector
  where type vector_slice = LargeIntVectorSlice.slice
  where type array = LargeIntArray.array
  where type elem = LargeInt.int = struct end

structure LargeWordArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = LargeWordVector.vector
  where type vector_slice = LargeWordVectorSlice.slice
  where type array = LargeWordArray.array
  where type elem = LargeWord.word = struct end

structure LargeRealArraySlice :> MONO_ARRAY_SLICE (* OPTIONAL *)
  where type vector = LargeRealVector.vector
  where type vector_slice = LargeRealVectorSlice.slice
  where type array = LargeRealArray.array
  where type elem = LargeReal.real = struct end
