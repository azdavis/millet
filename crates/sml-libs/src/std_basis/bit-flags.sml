(*!
 * The BIT_FLAGS signature defines a generic set of operations on an abstract representation of
 * system flags. It is typically included as part of the interface of substructures which provide a
 * set of options.
 *)
signature BIT_FLAGS (* OPTIONAL *) = sig
  (*!
   * This type is the abstract representation of a set of system flags.
   *)
  eqtype flags
  (*!
   * These functions convert between the abstract flags type and a bit-vector that is represented as
   * a system word. The interpretation of the bits is system-dependent, but follows the C language
   * binding for the host operating system. Note that there is no error checking on the fromWord
   * function's argument.
   *)
  val toWord : flags -> SysWord.word
  val fromWord : SysWord.word -> flags
  (*!
   * all represents the union of all flags. Note that this may well be a superset of the flags value
   * defined in a matching structure. For example, BIT_FLAGS is used to define the flags specified
   * by the POSIX standard; a POSIX-conforming operating system may provide additional flags that
   * will not be defined in the Posix structure but could be set in the all value.
   *)
  val all : flags
  (*!
   * flags l returns a value that represents the union of the flags in the list l. The expression
   * flags [] denotes the empty set.
   *)
  val flags : flags list -> flags
  (*!
   * intersect l returns a value that represents the intersection of the sets of flags in the list
   * l. The expression intersect [] denotes all.
   *)
  val intersect : flags list -> flags
  (*!
   * clear (fl1, fl2) returns the set of those flags in fl2 that are not set in fl1, i.e., the set
   * difference fl2 \ fl1. It is equivalent to: fromWord(SysWord.andb(SysWord.notb (toWord fl1),
   * toWord fl2))
   *)
  val clear : flags * flags -> flags
  (*!
   * allSet (fl1, fl2) returns true if all of the flags in fl1 are also in fl2 (i.e., this tests for
   * inclusion of fl1 in fl2).
   *)
  val allSet : flags * flags -> bool
  (*!
   * anySet (fl1, fl2) returns true if any of the flags in fl1 is also in fl2 (i.e., this tests for
   * non-empty intersection).
   *)
  val anySet : flags * flags -> bool
end
