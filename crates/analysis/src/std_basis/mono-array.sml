signature MONO_ARRAY = sig
  eqtype array
  type elem
  type vector
  val maxLen : int
  val array : int * elem -> array
  val fromList : elem list -> array
  val tabulate : int * (int -> elem) -> array
  val length : array -> int
  val sub : array * int -> elem
  val update : array * int * elem -> unit
  val vector : array -> vector
  val copy : {src : array, dst : array, di : int} -> unit
  val copyVec : {src : vector, dst : array, di : int} -> unit
  val appi : (int * elem -> unit) -> array -> unit
  val app : (elem -> unit) -> array -> unit
  val modifyi : (int * elem -> elem) -> array -> unit
  val modify : (elem -> elem) -> array -> unit
  val foldli : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
  val foldri : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
  val foldl : (elem * 'b -> 'b) -> 'b -> array -> 'b
  val foldr : (elem * 'b -> 'b) -> 'b -> array -> 'b
  val findi : (int * elem -> bool) -> array -> (int * elem) option
  val find : (elem -> bool) -> array -> elem option
  val exists : (elem -> bool) -> array -> bool
  val all : (elem -> bool) -> array -> bool
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
