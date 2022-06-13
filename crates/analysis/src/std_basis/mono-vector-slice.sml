signature MONO_VECTOR_SLICE = sig
  type elem
  type vector
  type slice
  val length : slice -> int
  val sub : slice * int -> elem
  val full : vector -> slice
  val slice : vector * int * int option -> slice
  val subslice : slice * int * int option -> slice
  val base : slice -> vector * int * int
  val vector : slice -> vector
  val concat : slice list -> vector
  val isEmpty : slice -> bool
  val getItem : slice -> (elem * slice) option
  val appi : (int * elem -> unit) -> slice -> unit
  val app : (elem -> unit) -> slice -> unit
  val mapi : (int * elem -> elem) -> slice -> vector
  val map : (elem -> elem) -> slice -> vector
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
