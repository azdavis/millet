signature MONO_VECTOR = sig
  type vector
  type elem
  val maxLen : int
  val fromList : elem list -> vector
  val tabulate : int * (int -> elem) -> vector
  val length : vector -> int
  val sub : vector * int -> elem
  val update : vector * int * elem -> vector
  val concat : vector list -> vector
  val appi : (int * elem -> unit) -> vector -> unit
  val app : (elem -> unit) -> vector -> unit
  val mapi : (int * elem -> elem) -> vector -> vector
  val map : (elem -> elem) -> vector -> vector
  val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
  val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
  val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
  val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
  val findi : (int * elem -> bool) -> vector -> (int * elem) option
  val find : (elem -> bool) -> vector -> elem option
  val exists : (elem -> bool) -> vector -> bool
  val all : (elem -> bool) -> vector -> bool
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
