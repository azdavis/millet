signature UNSAFE = sig
  structure CInterface : CINTERFACE
  structure Object : UNSAFE_OBJECT
  structure Poll : POLL
  structure Vector : UNSAFE_VECTOR
  structure Array : UNSAFE_ARRAY
  structure CharVector : UNSAFE_MONO_VECTOR where type elem = char
  structure CharArray : UNSAFE_MONO_ARRAY where type elem = char
  structure Word8Vector : UNSAFE_MONO_VECTOR where type elem = Word8.word
  structure Word8Array : UNSAFE_MONO_ARRAY where type elem = Word8.word
  structure Real64Array : UNSAFE_MONO_ARRAY where type elem = Real64.real
  val getHdlr : unit -> 'a Cont.cont
  val setHdlr : 'a Cont.cont -> unit
  val getVar : unit -> 'a
  val setVar : 'a -> unit
  val getPseudo : int -> 'a
  val setPseudo : ('a * int) -> unit
  val blastRead : Word8Vector.vector -> 'a
  val blastWrite : 'a -> Word8Vector.vector
  val boxed : 'a -> bool
  val cast : 'a -> 'b
  val pStruct : Object.object ref
  val topLevelCont : unit Cont.cont ref
end

structure Unsafe : UNSAFE = struct end
