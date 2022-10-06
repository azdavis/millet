(*!
These modules provide unsafe access to data structures and to runtime-system functions. Use of
Unsafe by an ML program can cause corruption of the ML execution, which may lead to program crashes
or other insecurity.
!*)
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
  (*!
  getHdlr () Get the current exception handler, expressed as an exn cont, that is, a continuation
  taking an exception as an argument. (There seems to be no reason for the type to be 'a cont in
  this signature.) Throwing an exn to this continuation will behave as if the exception were raised.
  There is nothing inherently unsafe about this (except the bogus use of 'a cont in the signature
  instead of exn cont. It is even possible to grab the handler, then leave the dynamic scope of the
  handler, then throw to it, with paradoxical but not unsafe effect.
  !*)
  val getHdlr : unit -> 'a Cont.cont
  (*!
  setHdlr k Install k, which should be an exn cont, as the current exception handler.
  !*)
  val setHdlr : 'a Cont.cont -> unit
  (*!
  getVar () SML/NJ has a special global variable, kept in registers upon most target architectures,
  used for various purposes by subsystems such as CML or the debugger. This function fetches the
  contents of the "var".
  !*)
  val getVar : unit -> 'a
  (*!
  setVar a Sets the value of the special global variable.
  !*)
  val setVar : 'a -> unit
  (*!
  getPseudo i Obsolete, do not use.
  !*)
  val getPseudo : int -> 'a
  (*!
  setPseudo (a, i) Obsolete, do not use.
  !*)
  val setPseudo : ('a * int) -> unit
  (*!
  blastRead vec Unpack a data structure from a string.
  !*)
  val blastRead : Word8Vector.vector -> 'a
  (*!
  blastWrite a Marshall a data structure of arbitrary type into a linear sequence of bytes.
  !*)
  val blastWrite : 'a -> Word8Vector.vector
  (*!
  boxed a Tell whether the representation of a is via a pointer.
  !*)
  val boxed : 'a -> bool
  (*!
  cast a View a as if it had another type.
  !*)
  val cast : 'a -> 'b
  (*!
  pStruct A pointer to the primitive core structure; for use in bootstrapping the interactive
  system.
  !*)
  val pStruct : Object.object ref
  (*!
  topLevelCont The continuation to which control will be thrown when an interrupt signal (SIGINT, on
  Unix) is received. Usually, the topLevelCont continuation resumes the interactive loop, but in
  principle users could install other continuation values in this variable.
  !*)
  val topLevelCont : unit Cont.cont ref
end

structure Unsafe : UNSAFE = struct end
