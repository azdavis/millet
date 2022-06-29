signature MONO_ARRAY2 (* OPTIONAL *) = sig
  eqtype array
  type elem
  type vector
  type region = { base : array, row : int, col : int, nrows : int option, ncols : int option }
  datatype traversal =
  datatype Array2.traversal
  val array : int * int * elem -> array
  val fromList : elem list list -> array
  val tabulate : traversal -> int * int * (int * int -> elem) -> array
  val sub : array * int * int -> elem
  val update : array * int * int * elem -> unit
  val dimensions : array -> int * int
  val nCols : array -> int
  val nRows : array -> int
  val row : array * int -> vector
  val column : array * int -> vector
  val copy : { src : region, dst : array, dst_row : int, dst_col : int } -> unit
  val appi : traversal -> (int * int * elem -> unit) -> region -> unit
  val app : traversal -> (elem -> unit) -> array -> unit
  val foldi : traversal -> (int * int * elem * 'b -> 'b) -> 'b -> region -> 'b
  val fold : traversal -> (elem * 'b -> 'b) -> 'b -> array -> 'b
  val modifyi : traversal -> (int * int * elem -> elem) -> region -> unit
  val modify : traversal -> (elem -> elem) -> array -> unit
end

structure Word8Array2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = Word8Vector.vector
  where type elem = Word8.word = struct end

structure CharArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = CharVector.vector
  where type elem = char = struct end

structure WideCharArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = WideCharVector.vector
  where type elem = WideChar.char = struct end

structure BoolArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = BoolVector.vector
  where type elem = bool = struct end

structure IntArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = IntVector.vector
  where type elem = int = struct end

structure WordArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = WordVector.vector
  where type elem = word = struct end

structure RealArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = RealVector.vector
  where type elem = real = struct end

structure LargeIntArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = LargeIntVector.vector
  where type elem = LargeInt.int = struct end

structure LargeWordArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = LargeWordVector.vector
  where type elem = LargeWord.word = struct end

structure LargeRealArray2 :> MONO_ARRAY2 (* OPTIONAL *)
  where type vector = LargeRealVector.vector
  where type elem = LargeReal.real = struct end
