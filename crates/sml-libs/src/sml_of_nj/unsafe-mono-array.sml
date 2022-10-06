(*!
For those of you that don't mind a little insecurity if only your array accesses don't have bounds
checks, here are some unchecked operations on arrays.

These operations work on ordinary monomorphic arrays, so you can mix checked and unchecked
operations on the same arrays.

In an ideal world (which we're working on, in principle) the compiler would let you use the ordinary
checked operations and then optimize the checks away in a safe and efficient manner.
!*)
signature UNSAFE_MONO_ARRAY = sig
  type array
  type elem
  (*!
  sub (a, i) Fetch the ith element of a. If i is negative or greater than or equal to the length of
  a, the results will be unpredictable and may corrupt the further execution of the ML program.
  !*)
  val sub : (array * int) -> elem
  (*!
  update (a, i, x) Store x into the ith element of a. If i is negative or greater than or equal to
  the length of a, the results will be unpredictable and may corrupt the further execution of the ML
  program.
  !*)
  val update : (array * int * elem) -> unit
  (*!
  create i Create an array of i elements, with every slot initialized to zero. If i is negative or
  greater than the largest array size that the system can represent the results will be
  unpredictable and may corrupt the further execution of the ML program.
  !*)
  val create : int -> array
end
