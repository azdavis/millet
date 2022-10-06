(*!
For those of you that don't mind a little insecurity if only your array accesses don't have bounds
checks, here are some unchecked operations on arrays.

These operations work on ordinary arrays, so you can mix checked and unchecked operations on the
same arrays.

In an ideal world (which we're working on, in principle) the compiler would let you use the ordinary
checked operations and then optimize the checks away in a safe and efficient manner.
!*)
signature UNSAFE_ARRAY = sig
  (*!
  sub (a, i) Fetch the ith element of a. If i is negative or greater than or equal to the length of
  a, the results will be unpredictable and may corrupt the further execution of the ML program.
  !*)
  val sub : ('a array * int) -> 'a
  (*!
  update (a, i, x) Store x into the ith element of a. If i is negative or greater than or equal to
  the length of a, the results will be unpredictable and may corrupt the further execution of the ML
  program.
  !*)
  val update : ('a array * int * 'a) -> unit
  (*!
  create (i, x) Create an array of i elements, with every slot initialized to x. If i is negative or
  greater than the largest array size that the system can represent the results will be
  unpredictable and may corrupt the further execution of the ML program.
  !*)
  val create : (int * 'a) -> 'a array
end
