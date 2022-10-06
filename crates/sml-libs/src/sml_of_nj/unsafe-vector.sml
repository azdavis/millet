(*!
For those of you that don't mind a little insecurity if only your vector accesses don't have bounds
checks, here are some unchecked operations on vectors.

These operations work on ordinary vectors, so you can mix checked and unchecked operations on the
same vectors.

In an ideal world (which we're working on, in principle) the compiler would let you use the ordinary
checked operations and then optimize the checks away in a safe and efficient manner.
!*)
signature UNSAFE_VECTOR = sig
  (*!
  sub (vec, i) Fetch the ith element of a. If i is negative or greater than or equal to the length
  of a, the results will be unpredictable and may corrupt the further execution of the ML program.
  !*)
  val sub : ('a vector * int) -> 'a
  (*!
  create (i, l) Create an array of i elements, initialized from the list l. If i is negative, or
  greater than the length of l, or greater than the largest array size that the system can
  represent, the results will be unpredictable and may corrupt the further execution of the ML
  program.
  !*)
  val create : (int * 'a list) -> 'a vector
end
