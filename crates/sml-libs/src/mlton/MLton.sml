structure MLton : sig
  (*!
   * eq (x, y) returns true if x and y are equal as pointers. For simple types like char, int, and
   * word, this is the same as equals. For arrays, datatypes, strings, tuples, and vectors, this is
   * a simple pointer equality. The semantics is a bit murky.
   *)
  val eq : 'a * 'a -> bool
  (*!
   * equal (x, y) returns true if x and y are structurally equal. For equality types, this is the
   * same as [PolymorphicEquality](http://mlton.org/PolymorphicEquality). For other types, it is a
   * conservative approximation of equivalence.
   *)
  val equal : 'a * 'a -> bool
  (*!
   * hash x returns a structural hash of x. The hash function is consistent between execution of the
   * same program, but may not be consistent between different programs.
   *)
  val hash : 'a -> Word32.word
  (*!
   * isMLton is always true in a MLton implementation, and is always false in a stub implementation.
   *)
  val isMLton : bool
  (*!
   * share x maximizes sharing in the heap for the object graph reachable from x.
   *)
  val share : 'a -> unit
  (*!
   * shareAll () maximizes sharing in the heap by sharing space for equivalent immutable objects. A
   * call to shareAll performs a major garbage collection, and takes time proportional to the size
   * of the heap.
   *)
  val shareAll : unit -> unit
  (*!
   * size x returns the amount of heap space (in bytes) taken by the value of x, including all
   * objects reachable from x by following pointers. It takes time proportional to the size of x.
   * See below for an example.
   *)
  val size : 'a -> IntInf.int
  (*!
   * sizeAll () returns the amount of heap space (in bytes) taken by all reachable live data. It
   * takes time proportional to the size of live data.
   *)
  val sizeAll : 'a -> IntInf.int
  structure Array : MLTON_ARRAY
  structure BinIO : MLTON_BIN_IO
  structure CharArray : MLTON_MONO_ARRAY where type t = CharArray.array where type elem = CharArray.elem
  structure CharVector : MLTON_MONO_VECTOR where type t = CharVector.vector where type elem = CharVector.elem
  structure Cont : MLTON_CONT
  structure Exn : MLTON_EXN
  structure Finalizable : MLTON_FINALIZABLE
  structure GC : MLTON_GC
  structure IntInf : MLTON_INT_INF
  structure Itimer : MLTON_ITIMER
  structure LargeReal : MLTON_REAL where type t = LargeReal.real
  structure LargeWord : MLTON_WORD where type t = LargeWord.word
  structure Platform : MLTON_PLATFORM
  structure Pointer : MLTON_POINTER
  structure ProcEnv : MLTON_PROC_ENV
  structure Process : MLTON_PROCESS
  structure Profile : MLTON_PROFILE
  structure Random : MLTON_RANDOM
  structure Real : MLTON_REAL where type t = Real.real
  structure Real32 : sig
    include MLTON_REAL
    val castFromWord : Word32.word -> t
    val castToWord : t -> Word32.word
  end where type t = Real32.real
  structure Real64 : sig
    include MLTON_REAL
    val castFromWord : Word64.word -> t
    val castToWord : t -> Word64.word
  end where type t = Real64.real
  structure Rlimit : MLTON_RLIMIT
  structure Rusage : MLTON_RUSAGE
  structure Signal : MLTON_SIGNAL
  structure Syslog : MLTON_SYSLOG
  structure TextIO : MLTON_TEXT_IO
  structure Thread : MLTON_THREAD
  structure Vector : MLTON_VECTOR
  structure Weak : MLTON_WEAK
  structure Word : MLTON_WORD where type t = Word.word
  structure Word8 : MLTON_WORD where type t = Word8.word
  structure Word16 : MLTON_WORD where type t = Word16.word
  structure Word32 : MLTON_WORD where type t = Word32.word
  structure Word64 : MLTON_WORD where type t = Word64.word
  structure Word8Array : MLTON_MONO_ARRAY where type t = Word8Array.array where type elem = Word8Array.elem
  structure Word8Vector : MLTON_MONO_VECTOR where type t = Word8Vector.vector where type elem = Word8Vector.elem
  structure World : MLTON_WORLD
end = struct end
