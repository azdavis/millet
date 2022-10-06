(*!
For those of you that don't mind a little insecurity if only your vector accesses don't have bounds
checks, here are some unchecked operations on vectors.

These operations work on ordinary vectors, so you can mix checked and unchecked operations on the
same vectors.

In an ideal world (which we're working on, in principle) the compiler would let you use the ordinary
checked operations and then optimize the checks away in a safe and efficient manner.
!*)
signature UNSAFE_MONO_VECTOR = sig
  type vector
  type elem
  (*!
  sub (vec, i) Fetch the ith element of a. If i is negative or greater than or equal to the length
  of a, the results will be unpredictable and may corrupt the further execution of the ML program.
  !*)
  val sub : (vector * int) -> elem
  (*!
  update (vec, i, el) Store x into the ith element of a. If i is negative or greater than or equal
  to the length of a, the results will be unpredictable and may corrupt the further execution of the
  ML program. This operation is particularly unsafe, even if i is in range, because the compiler
  performs other optimizations on vectors based on the fact that their contents are immutable.
  !*)
  val update : (vector * int * elem) -> unit
  (*!
  create i Create an array of i elements, with every slot uninitialized. If i is negative or greater
  than the largest array size that the system can represent the results will be unpredictable and
  may corrupt the further execution of the ML program.
  !*)
  val create : int -> vector
end
