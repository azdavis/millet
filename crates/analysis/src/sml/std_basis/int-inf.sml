(*!
The optional IntInf structure is one of the possible implementations of the INTEGER interface. In
addition to the INTEGER operations, it provides some operations useful for programming with
arbitrarily large integers. Operations in IntInf that return a value of type IntInf.int should never
raise the Overflow exception. Note that, as it extends the INTEGER interface, IntInf defines a type
int. Any use of this type below, unmodified by a structure, refers to the local type int defined in
IntInf.
!*)
signature INT_INF (* OPTIONAL *) = sig
  include INTEGER
  (*!
  divMod (i, j) returns the pair (i div j, i mod j), but is likely to be more efficient than
  computing both components separately. It raises Div if j = 0.
  !*)
  val divMod : int * int -> int * int
  (*!
  quotRem (i, j) returns the pair (i quot j, i rem j), but is likely to be more efficient than
  computing both components separately. It raises Div if j = 0.
  !*)
  val quotRem : int * int -> int * int
  (*!
  pow (i, j) returns the result of raising i to the j(th) power. This is well-defined when j > 0.
  When j = 0, pow(i, j) is 1; in particular, pow(0, 0) is 1. When j < 0, we define the following
  exceptional cases: i pow(i,j) 0 Raise Div |i| = 1 i(j) |i| > 1 0
  !*)
  val pow : int * Int.int -> int
  (*!
  log2 i returns the truncated base-2 logarithm of its argument, i.e., the largest integer k for
  which pow(2, k) <= i. It raises Domain if i <= 0 and Overflow if the result is not representable
  as an Int.int.
  !*)
  val log2 : int -> Int.int
  (*!
  These functions return the bit-wise OR, bit-wise exclusive OR, and bit-wise AND, respectively, of
  the arguments.
  !*)
  val orb : int * int -> int
  val xorb : int * int -> int
  val andb : int * int -> int
  (*!
  notb i returns the bit-wise complement (NOT) of i. It is equivalent to ~(i + 1).
  !*)
  val notb : int -> int
  (*!
  << (i, n) shifts i to the left by n bit positions, filling in zeros from the right. When i and n
  are interpreted as integers, the latter non-negative, this returns (i * 2(n)).
  !*)
  val << : int * Word.word -> int
  (*!
  ~>> (i, n) shifts i to the right by n bit positions. When i and n are interpreted as integers, the
  latter non-negative, this returns floor(((i / 2(n)))).
  !*)
  val ~>> : int * Word.word -> int
end

structure IntInf :> INT_INF (* OPTIONAL *) = struct end
