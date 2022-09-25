(* incomplete *)
signature UNSAFE = sig
  (*!
   * SML/NJ has a special global variable, kept in registers upon most target architectures, used
   * for various purposes by subsystems such as CML or the debugger. This function fetches the
   * contents of the "var".
   *)
  val getVar : unit -> 'a
  (*!
   * Sets the value of the special global variable.
   *)
  val setVar : 'a -> unit
  (*!
   * Unpack a data structure from a string.
   *)
  val blastRead : Word8Vector.vector -> 'a
  (*!
   * Marshall a data structure of arbitrary type into a linear sequence of bytes.
   *)
  val blastWrite : 'a -> Word8Vector.vector
  (*!
   * Tell whether the representation of a is via a pointer.
   *)
  val boxed : 'a -> bool
  (*!
   * View a as if it had another type.
   *)
  val cast : 'a -> 'b
end

structure Unsafe : UNSAFE = struct end
