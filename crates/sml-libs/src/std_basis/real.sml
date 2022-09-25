structure LargeReal :> sig type real end = struct end

(*!
 * The REAL signature specifies structures that implement floating-point numbers. The semantics of
 * floating-point numbers should follow the IEEE standard 754-1985 [CITE] and the ANSI/IEEE standard
 * 854-1987[CITE]. In addition, implementations of the REAL signature are required to use
 * non-trapping semantics. Additional aspects of the design of the REAL and MATH signatures were
 * guided by the Floating-Point C Extensions[CITE] developed by the X3J11 ANSI committee and the
 * lecture notes [CITE] by W. Kahan on the IEEE standard 754.
 *
 * Although there can be many representations for NaN values, the Library models them as a single
 * value and currently provides no explicit way to distinguish among them, ignoring the sign bit.
 * Thus, in the descriptions below and in the Math structure, we just refer to the NaN value.
 *)
signature REAL = sig
  (*!
   * Note that, as discussed below, real is not an equality type.
   *)
  type real
  structure Math : MATH
    where type real = real
  (*!
   * The base of the representation, e.g., 2 or 10 for IEEE floating point.
   *)
  val radix : int
  (*!
   * The number of digits, each between 0 and radix-1, in the mantissa. Note that the precision
  includes the implicit (or hidden) bit used in the IEEE representation (e.g., the value of
  Real64.precision is 53).
   *)
  val precision : int
  (*!
   * The maximum finite number, the minimum non-zero positive number, and the minimum non-zero
   * normalized number, respectively.
   *)
  val maxFinite : real
  val minPos : real
  val minNormalPos : real
  (*!
   * Positive and negative infinity values.
   *)
  val posInf : real
  val negInf : real
  val + : real * real -> real
  val - : real * real -> real
  (*!
   * r1 * r2 denotes the product of r1 and r2. The product of zero and an infinity produces NaN.
   * Otherwise, if one argument is infinite, the result is infinite with the correct sign, e.g., -5
   * * (-infinity) = infinity, infinity * (-infinity) = -infinity.
   *)
  val * : real * real -> real
  (*!
   * r1 / r2 denotes the quotient of r1 and r2. We have 0 / 0 = NaN and +-infinity / +-infinity =
   * NaN. Dividing a finite, non-zero number by a zero, or an infinity by a finite number produces
   * an infinity with the correct sign. (Note that zeros are signed.) A finite number divided by an
   * infinity is 0 with the correct sign.
   *)
  val / : real * real -> real
  (*!
   * rem (x, y) returns the remainder x - n*y, where n = trunc (x / y). The result has the same sign
   * as x and has absolute value less than the absolute value of y. If x is an infinity or y is 0,
   * rem returns NaN. If y is an infinity, rem returns x.
   *)
  val rem : real * real -> real
  (*!
   * These return a*b + c and a*b - c, respectively. Their behaviors on infinities follow from the
   * behaviors derived from addition, subtraction, and multiplication. The precise semantics of
   * these operations depend on the language implementation and the underlying hardware.
   * Specifically, certain architectures provide these operations as a single instruction, possibly
   * using a single rounding operation. Thus, the use of these operations may be faster than
   * performing the individual arithmetic operations sequentially, but may also cause different
   * rounding behavior.
   *)
  val *+ : real * real * real -> real
  (*!
   * See *+.
   *)
  val *- : real * real * real -> real
  (*!
   * ~ r produces the negation of r. ~ (+-infinity) = -+infinity.
   *)
  val ~ : real -> real
  (*!
   * abs r returns the absolute value |r| of r. abs (+-0.0) = +0.0 abs (+-infinity) = +infinity abs
   * (+-NaN) = +NaN
   *)
  val abs : real -> real
  (*!
   * These return the smaller (respectively, larger) of the arguments. If exactly one argument is
   * NaN, they return the other argument. If both arguments are NaN, they return NaN.
   *)
  val min : real * real -> real
  val max : real * real -> real
  (*!
   * sign r returns ~1 if r is negative, 0 if r is zero, or 1 if r is positive. An infinity returns
   * its sign; a zero returns 0 regardless of its sign. It raises Domain on NaN.
   *)
  val sign : real -> int
  (*!
   * signBit r returns true if and only if the sign of r (infinities, zeros, and NaN, included) is
   * negative.
   *)
  val signBit : real -> bool
  (*!
   * sameSign (r1, r2) returns true if and only if signBit r1 equals signBit r2.
   *)
  val sameSign : real * real -> bool
  (*!
   * copySign (x, y) returns x with the sign of y, even if y is NaN.
   *)
  val copySign : real * real -> real
  (*!
   * The function compare returns LESS, EQUAL, or GREATER according to whether its first argument is
   * less than, equal to, or greater than the second. It raises IEEEReal.Unordered on unordered
   * arguments. The function compareReal behaves similarly except that the values it returns have
   * the extended type IEEEReal.real_order and it returns IEEEReal.UNORDERED on unordered arguments.
   * Implementation note: Implementations should try to optimize use of compare, since it is
   * necessary for catching NaNs.
   *)
  val compare : real * real -> order
  val compareReal : real * real -> IEEEReal.real_order
  (*!
   * These return true if the corresponding relation holds between the two reals. Note that these
   * operators return false on unordered arguments, i.e., if either argument is NaN, so that the
   * usual reversal of comparison under negation does not hold, e.g., a < b is not the same as not
   * (a >= b).
   *)
  val < : real * real -> bool
  val <= : real * real -> bool
  val > : real * real -> bool
  val >= : real * real -> bool
  (*!
   * The first returns true if and only if neither y nor x is NaN, and y and x are equal, ignoring
   * signs on zeros. This is equivalent to the IEEE = operator. The second function != is equivalent
   * to not o op == and the IEEE ?<> operator.
   *)
  val == : real * real -> bool
  (*!
   * See ==.
   *)
  val != : real * real -> bool
  (*!
   * This returns true if either argument is NaN or if the arguments are bitwise equal, ignoring
   * signs on zeros. It is equivalent to the IEEE ?= operator.
   *)
  val ?= : real * real -> bool
  (*!
   * unordered (x, y) returns true if x and y are unordered, i.e., at least one of x and y is NaN.
   *)
  val unordered : real * real -> bool
  (*!
   * isFinite x returns true if x is neither NaN nor an infinity.
   *)
  val isFinite : real -> bool
  (*!
   * isNan x returns true if x is NaN.
   *)
  val isNan : real -> bool
  (*!
   * isNormal x returns true if x is normal, i.e., neither zero, subnormal, infinite nor NaN.
   *)
  val isNormal : real -> bool
  (*!
   * class x returns the IEEEReal.float_class to which x belongs.
   *)
  val class : real -> IEEEReal.float_class
  (*!
   * toManExp r returns {man, exp}, where man and exp are the mantissa and exponent of r,
   * respectively. Specifically, we have the relation r = man * radix(exp) where 1.0 <= man * radix
   * < radix. This function is comparable to frexp in the C library. If r is +-0, man is +-0 and exp
   * is +0. If r is +-infinity, man is +-infinity and exp is unspecified. If r is NaN, man is NaN
   * and exp is unspecified.
   *)
  val toManExp : real -> {man : real, exp : int}
  (*!
   * fromManExp {man, exp} returns man * radix(exp). This function is comparable to ldexp in the C
   * library. Note that, even if man is a non-zero, finite real value, the result of fromManExp can
   * be zero or infinity because of underflows and overflows. If man is +-0, the result is +-0. If
   * man is +-infinity, the result is +-infinity. If man is NaN, the result is NaN.
   *)
  val fromManExp : {man : real, exp : int} -> real
  (*!
   * The former returns {whole, frac}, where frac and whole are the fractional and integral parts of
   * r, respectively. Specifically, whole is integral, |frac| < 1.0, whole and frac have the same
   * sign as r, and r = whole + frac. This function is comparable to modf in the C library. If r is
   * +-infinity, whole is +-infinity and frac is +-0. If r is NaN, both whole and frac are NaN.
   * realMod is equivalent to #frac o split.
   *)
  val split : real -> {whole : real, frac : real}
  (*!
   * See split.
   *)
  val realMod : real -> real
  (*!
   * nextAfter (r, t) returns the next representable real after r in the direction of t. Thus, if t
   * is less than r, nextAfter returns the largest representable floating-point number less than r.
   * If r = t then it returns r. If either argument is NaN, this returns NaN. If r is +-infinity, it
   * returns +-infinity.
   *)
  val nextAfter : real * real -> real
  (*!
   * checkFloat x raises Overflow if x is an infinity, and raises Div if x is NaN. Otherwise, it
   * returns its argument. This can be used to synthesize trapping arithmetic from the non-trapping
   * operations given here. Note, however, that infinities can be converted to NaNs by some
   * operations, so that if accurate exceptions are required, checks must be done after each
   * operation.
   *)
  val checkFloat : real -> real
  (*!
   * These functions convert real values to integer-valued reals. realFloor produces floor(r), the
   * largest integer not larger than r. realCeil produces ceil(r), the smallest integer not less
   * than r. realTrunc rounds r towards zero, and realRound rounds to the integer-values real value
   * that is nearest to r. If r is NaN or an infinity, these functions return r.
   *)
  val realFloor : real -> real
  (*!
   * See realFloor.
   *)
  val realCeil : real -> real
  (*!
   * See realFloor.
   *)
  val realTrunc : real -> real
  (*!
   * See realFloor.
   *)
  val realRound : real -> real
  (*!
   * These functions convert reals to integers. floor produces floor(r), the largest int not larger
   * than r. ceil produces ceil(r), the smallest int not less than r. trunc rounds r towards zero.
   * round yields the integer nearest to r. In the case of a tie, it rounds to the nearest even
   * integer. They raise Overflow if the resulting value cannot be represented as an int, for
   * example, on infinity. They raise Domain on NaN arguments. These are respectively equivalent to:
   * toInt IEEEReal.TO_NEGINF r toInt IEEEReal.TO_POSINF r toInt IEEEReal.TO_ZERO r toInt
   * IEEEReal.TO_NEAREST r
   *)
  val floor : real -> int
  (*!
   * See floor.
   *)
  val ceil : real -> int
  (*!
   * See floor.
   *)
  val trunc : real -> int
  (*!
   * See floor.
   *)
  val round : real -> int
  (*!
   * These functions convert the argument x to an integral type using the specified rounding mode.
   * They raise Overflow if the result is not representable, in particular, if x is an infinity.
   * They raise Domain if the input real is NaN.
   *)
  val toInt : IEEEReal.rounding_mode -> real -> int
  (*!
   * See toInt.
   *)
  val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int
  (*!
   * These functions convert the integer i to a real value. If the absolute value of i is larger
   * than maxFinite, then the appropriate infinity is returned. If i cannot be exactly represented
   * as a real value, then the current rounding mode is used to determine the resulting value. The
   * top-level function real is an alias for Real.fromInt.
   *)
  val fromInt : int -> real
  (*!
   * See fromInt.
   *)
  val fromLargeInt : LargeInt.int -> real
  (*!
   * These convert between values of type real and type LargeReal.real. If r is too small or too
   * large to be represented as a real, fromLarge will convert it to a zero or an infinity.
   *)
  val toLarge : real -> LargeReal.real
  (*!
   * See toLarge.
   *)
  val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real
  (*!
   * These functions convert reals into strings. The conversion provided by the function fmt is
   * parameterized by spec, which has the following forms and interpretations. SCI arg Scientific
   * notation: [~]?[0-9].[0-9]+?E[0-9]+ where there is always one digit before the decimal point,
   * nonzero if the number is nonzero. arg specifies the number of digits to appear after the
   * decimal point, with 6 the default if arg is NONE. If arg is SOME(0), no fractional digits and
   * no decimal point are printed. FIX arg Fixed-point notation: [~]?[0-9]+.[0-9]+? arg specifies
   * the number of digits to appear after the decimal point, with 6 the default if arg is NONE. If
   * arg is SOME(0), no fractional digits and no decimal point are printed. GEN arg Adaptive
   * notation: the notation used is either scientific or fixed-point depending on the value
   * converted. arg specifies the maximum number of significant digits used, with 12 the default if
   * arg is NONE. EXACT Exact decimal notation: refer to IEEEReal.toString for a complete
   * description of this format. In all cases, positive and negative infinities are converted to
   * "inf" and "~inf", respectively, and NaN values are converted to the string "nan". Refer to
   * StringCvt.realfmt for more details concerning these formats, especially the adaptive format
   * GEN. fmt raises Size if spec is an invalid precision, i.e., if spec is SCI (SOME i) with i < 0
   * FIX (SOME i) with i < 0 GEN (SOME i) with i < 1 The exception should be raised when fmt spec is
   * evaluated. The fmt function allows the user precise control as to the form of the resulting
   * string. Note, therefore, that it is possible for fmt to produce a result that is not a valid
   * SML string representation of a real value. The value returned by toString is equivalent to:
   * (fmt (StringCvt.GEN NONE) r)
   *)
  val fmt : StringCvt.realfmt -> real -> string
  (*!
   * See fmt.
   *)
  val toString : real -> string
  (*!
   * These functions scan a real value from character source. The first version reads from ARG/strm/
   * using reader getc, ignoring initial whitespace. It returns SOME(r,rest) if successful, where r
   * is the scanned real value and rest is the unused portion of the character stream strm. Values
   * of too large a magnitude are represented as infinities; values of too small a magnitude are
   * represented as zeros. The second version returns SOME(r) if a real value can be scanned from a
   * prefix of s, ignoring any initial whitespace; otherwise, it returns NONE. This function is
   * equivalent to StringCvt.scanString scan. The functions accept real numbers with the following
   * format: [+~-]?([0-9]+.[0-9]+? | .[0-9]+)(e | E)[+~-]?[0-9]+? It also accepts the following
   * string representations of non-finite values: [+~-]?(inf | infinity | nan) where the alphabetic
   * characters are case-insensitive.
   *)
  val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
  (*!
   * See scan.
   *)
  val fromString : string -> real option
  (*!
   * These convert between real values and decimal approximations. Decimal approximations are to be
   * converted using the IEEEReal.TO_NEAREST rounding mode. toDecimal should produce only as many
   * digits as are necessary for fromDecimal to convert back to the same number. In particular, for
   * any normal or subnormal real value r, we have the bit-wise equality: fromDecimal (toDecimal r)
   * = r. For toDecimal, when the r is not normal or subnormal, then the exp field is set to 0 and
   * the digits field is the empty list. In all cases, the sign and class field capture the sign and
   * class of r. For fromDecimal, if class is ZERO or INF, the resulting real is the appropriate
   * signed zero or infinity. If class is NAN, a signed NaN is generated. If class is NORMAL or
   * SUBNORMAL, the sign, digits and exp fields are used to produce a real number whose value is. s
   * * 0.d(1)d(2)...d(n) 10(exp) where digits = [d(1), d(2), ..., d(n)] and where s is -1 if sign is
   * true and 1 otherwise. Note that the conversion itself should ignore the class field, so that
   * the resulting value might have class NORMAL, SUBNORMAL, ZERO, or INF. For example, if digits is
   * empty or a list of all 0's, the result should be a signed zero. More generally, very large or
   * small magnitudes are converted to infinities or zeros. If the argument to fromDecimal does not
   * have a valid format, i.e., if the digits field contains integers outside the range [0,9], it
   * returns NONE. Implementation note: Algorithms for accurately and efficiently converting between
   * binary and decimal real representations are readily available, e.g., see the technical report
   * by Gay[CITE].
   *)
  val toDecimal : real -> IEEEReal.decimal_approx
  (*!
   * See toDecimal.
   *)
  val fromDecimal : IEEEReal.decimal_approx -> real option
end

structure Real :> REAL
  where type real = real = Real
structure LargeReal :> REAL
  where type real = LargeReal.real = LargeReal
structure Real64 (* OPTIONAL *) = LargeReal

(*
 * Alias for `Real.ceil`.
 *)
val ceil = Real.ceil
(*
 * Alias for `Real.floor`.
 *)
val floor = Real.floor
(*
 * Alias for `Real.fromInt`.
 *)
val real = Real.fromInt
(*
 * Alias for `Real.round`.
 *)
val round = Real.round
(*
 * Alias for `Real.trunc`.
 *)
val trunc = Real.trunc
