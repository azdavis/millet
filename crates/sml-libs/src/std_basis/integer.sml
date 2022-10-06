structure Int :> sig type int = int end = struct end
structure LargeInt :> sig type int end = struct end

(*!
 * Instances of the INTEGER signature provide a type of signed integers of either a fixed or
 * arbitrary precision, and arithmetic and conversion operations. For fixed precision
 * implementations, most arithmetic operations raise the exception Overflow when their result is not
 * representable.
 *)
signature INTEGER = sig
  eqtype int
  (*!
   * These convert between integer values of types int and LargeInt.int. The latter raises Overflow
   * if the value does not fit. Int<M>.fromLarge o Int<N>.toLarge converts an integer from type
   * Int<N>.int to Int<M>.int.
   *)
  val toLarge : int -> LargeInt.int
  val fromLarge : LargeInt.int -> int
  (*!
   * These convert between integer values of types int and the default integer type. They raise
   * Overflow if the value does not fit.
   *)
  val toInt : int -> Int.int
  val fromInt : Int.int -> int
  (*!
   * If SOME(n), this denotes the number n of significant bits in type int, including the sign bit.
   * If it is NONE, int has arbitrary precision. The precision need not necessarily be a power of
   * two.
   *)
  val precision : Int.int option
  (*!
   * The minimal (most negative) and the maximal (most positive) integers, respectively,
   * representable by int. If a value is NONE, int can represent all negative (respectively,
   * positive) integers, within the limits of the heap size. If precision is SOME(n), then we have
   * minInt = -2(n-1) and maxInt = 2(n-1) - 1.
   *)
  val minInt : int option
  val maxInt : int option
  (*!
   * These functions return the sum, difference, and product, respectively, of the arguments. They
   * raise Overflow when the result is not representable.
   *)
  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  (*!
   * i div j returns the greatest integer less than or equal to the quotient of i by j, i.e.,
   * floor(((i / j))). It raises Overflow when the result is not representable, or Div when j = 0.
   * Note that rounding is towards negative infinity, not zero.
   *)
  val div : int * int -> int
  (*!
   * i mod j returns the remainder of the division of i by j. It raises Div when j = 0. When
   * defined, (i mod j) has the same sign as j, and (i div j) * j + (i mod j) = i
   *)
  val mod : int * int -> int
  (*!
   * quot (i, j) returns the truncated quotient of the division of i by j, i.e., it computes (i / j)
   * and then drops any fractional part of the quotient. It raises Overflow when the result is not
   * representable, or Div when j = 0. Note that unlike div, quot rounds towards zero. In addition,
   * unlike div and mod, neither quot nor rem are infix by default; an appropriate infix declaration
   * would be infix 7 quot rem. Implementation note: This is the semantics of most hardware divide
   * instructions, so quot may be faster than div.
   *)
  val quot : int * int -> int
  (*!
   * i rem j returns the remainder of the division of i by j. It raises Div when j = 0. (i rem j)
   * has the same sign as i, and it holds that (i quot j) * j + (i rem j) = i This is the semantics
   * of most hardware divide instructions, so rem may be faster than mod.
   *)
  val rem : int * int -> int
  (*!
   * compare (i, j) returns LESS, EQUAL, or GREATER when i is less than, equal to, or greater than
   * j, respectively.
   *)
  val compare : int * int -> order
  (*!
   * These return true if the corresponding relation holds between the two integers.
   *)
  val < : int * int -> bool
  val <= : int * int -> bool
  val > : int * int -> bool
  val >= : int * int -> bool
  (*!
   * ~ i returns the negation of i, i.e., (0 - i). It raises Overflow when the result is not
   * representable. This can happen, for example, when int is an n-bit 2's-complement integer type,
   * and ~ is applied to -2 (n-1).
   *)
  val ~ : int -> int
  (*!
   * abs i returns the absolute value (magnitude) of i. It raises Overflow when the result is not
   * representable.
   *)
  val abs : int -> int
  (*!
   * These return the smaller (respectively, larger) of the arguments.
   *)
  val min : int * int -> int
  val max : int * int -> int
  (*!
   * sign i returns ~1, 0, or 1 when i is less than, equal to, or greater than 0, respectively.
   *)
  val sign : int -> Int.int
  (*!
   * sameSign (i, j) returns true if i and j have the same sign. It is equivalent to (sign i = sign
   * j).
   *)
  val sameSign : int * int -> bool
  (*!
   * These return a string containing a representation of i with #"~" used as the sign for negative
   * numbers. The former formats the string according to radix, The hexadecimal digits 10 through 15
   * are represented as #"A" through #"F", respectively. No prefix "0x" is generated for the
   * hexadecimal representation. The second form is equivalent to fmt StringCvt.DEC i.
   *)
  val fmt : StringCvt.radix -> int -> string
  (*!
   * See fmt.
   *)
  val toString : int -> string
  (*!
   * The first expression returns SOME(i,rest) if an integer in the format denoted by radix can be
   * parsed from a prefix of the character stream strm after skipping initial whitespace, where i is
   * the value of the integer parsed and rest is the rest of the character stream. NONE is returned
   * otherwise. This function raises Overflow when an integer can be parsed, but is too large to be
   * represented by type int. The format that scan accepts depends on the radix argument. Regular
   * expressions defining these formats are as follows: Radix Format StringCvt.BIN [+~-]?[0-1]+
   * StringCvt.OCT [+~-]?[0-7]+ StringCvt.DEC [+~-]?[0-9]+ StringCvt.HEX [+~-]?(0x |
   * 0X)?[0-9a-fA-F]+ Note that strings such as "0xg" and "0x 123" are scanned as SOME(0), even
   * using a hexadecimal radix. The second expression returns SOME(i) if an integer i in the format
   * [+~-]?[0-9]+ can be parsed from a prefix of the string s, ignoring initial whitespace; NONE is
   * returned otherwise. The function fromString raises Overflow when an integer can be parsed, but
   * is too large to fit in type int. It is equivalent to the expression StringCvt.scanString (scan
   * StringCvt.DEC).
   *)
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
  (*!
   * See scan.
   *)
  val fromString : string -> int option
end

structure Int :> INTEGER
  where type int = int = Int
structure Int32 :> INTEGER (* OPTIONAL *) = struct end
structure FixedInt :> INTEGER (* OPTIONAL *) = struct end
structure Position :> INTEGER = struct end
