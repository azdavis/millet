(*!
 * The StringCvt structure provides types and functions for handling the conversion between strings
   and values of various basic types.
 *)
signature STRING_CVT = sig
  (*!
   * The values of type radix are used to specify the radix of a representation of an integer,
   * corresponding to the bases 2, 8, 10, and 16, respectively.
   *)
  datatype radix = BIN | OCT | DEC | HEX
  (*!
   * Values of type realfmt are used to specify the format of a string representation for a real or
   * floating-point number. The first corresponds to scientific representation:
   * [~]?[0-9].[0-9]+?E[0-9]+ where there is always one digit before the decimal point, nonzero if
   * the number is nonzero. The optional integer value specifies the number of decimal digits to
   * appear after the decimal point, with 6 being the default. In particular, if 0 is specified,
   * there should be no fractional part. The exponent is zero if the value is zero. The second
   * corresponds to a fixed-point representation: [~]?[0-9]+.[0-9]+? where there is always at least
   * one digit before the decimal point. The optional integer value specifies the number of decimal
   * digits to appear after the decimal point, with 6 being the default. In particular, if 0 is
   * specified, there should be no fractional part. The third constructor GEN allows a formatting
   * function to use either the scientific or fixed-point notation, whichever is shorter, breaking
   * ties in favor of fixed-point. The optional integer value specifies the maximum number of
   * significant digits used, with 12 the default. The string should display as many significant
   * digits as possible, subject to this maximum. There should not be any trailing zeros after the
   * decimal point. There should not be a decimal point unless a fractional part is included.
   * [FLOAT] provides a reference implementation for this conversion. The fourth constructor EXACT
   * specifies that the string should represent the real using an exact decimal representation. The
   * string contains enough information in order to reconstruct a semantically equivalent real value
   * using REAL.fromDecimal o valOf o IEEEReal.fromString. Refer to the description of
   * IEEEReal.toString for more precise information concerning this format. In all cases, positive
   * and negative infinities are converted to "inf" and "~inf", respectively, and NaN values are
   * converted to the string "nan".
   *)
  datatype realfmt = SCI of int option | FIX of int option | GEN of int option | EXACT
  (*!
   * The type of a reader producing values of type 'a from a stream of type 'b. A return value of
   * SOME(a,b) corresponds to a value a scanned from the stream, plus the remainder b of the stream.
   * A return value of NONE indicates that no value of the correct type could be scanned from the
   * prefix of the stream. The reader type is designed for use with a stream or functional view of
   * I/O. Scanning functions using the reader type, such as skipWS, splitl, and Int.scan, will often
   * use lookahead characters to determine when to stop scanning. If the character source ('b in an
   * ('a,'b) reader) is imperative, the lookahead characters will be lost to any subsequent scanning
   * of the source. One mechanism for combining imperative I/O with the standard scanning functions
   * is provided by the TextIO.scanStream function.
   *)
  type ('a,'b) reader = 'b -> ('a * 'b) option
  (*!
   * These return s padded, on the left or right, respectively, with i - |s| copies of the character
   * c. If |s| >= i, they just return the string s. In other words, these functions right and
   * left-justify s in a field i characters wide, never trimming off any part of s. Note that if i
   * <= 0, s is returned. These functions raise Size if the size of the resulting string would be
   * greater than String.maxSize.
   *)
  val padLeft : char -> int -> string -> string
  (*!
   * See padLeft.
   *)
  val padRight : char -> int -> string -> string
  (*!
   * splitl f rdr src returns (pref, src') where pref is the longest prefix (left substring) of src,
   * as produced by the character reader rdr, all of whose characters satisfy f, and src' is the
   * remainder of src. Thus, the first character retrievable from src' is the leftmost character not
   * satisfying f. splitl can be used with scanning functions such as scanString by composing it
   * with SOME; e.g., scanString (fn rdr => SOME o (splitl f rdr)).
   *)
  val splitl : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
  (*!
   * These routines scan the source src for the first character not satisfying the predicate f. The
   * function dropl drops the maximal prefix consisting of characters satisfying the predicate,
   * returning the rest of the source, while takel returns the maximal prefix consisting of
   * characters satisfying the predicate. These can be defined in terms of splitl: takel f rdr s =
   * #1(splitl f rdr s) dropl f rdr s = #2(splitl f rdr s)
   *)
  val takel : (char -> bool) -> (char, 'a) reader -> 'a -> string
  (*!
   * See takel.
   *)
  val dropl : (char -> bool) -> (char, 'a) reader -> 'a -> 'a
  (*!
   * skipWS rdr src strips whitespace characters from a stream src using the reader rdr. It returns
   * the remaining stream. A whitespace character is one that satisfies the predicate Char.isSpace.
   * It is equivalent to dropl Char.isSpace.
   *)
  val skipWS : (char, 'a) reader -> 'a -> 'a
  (*!
   * The abstract type of the character stream used by scanString. A value of this type represents
   * the state of a character stream. The concrete type is left unspecified to allow implementations
   * a choice of representations. Typically, cs will be an integer index into a string.
   *)
  type cs
  (*!
   * The function scanString provides a general framework for converting a string into some value.
   * The user supplies a scanning function and a string. scanString converts the string into a
   * character source (type cs) and applies the scanning function. A scanning function converts a
   * reader of characters into a reader of values of the desired type. Typical scanning functions
   * are Bool.scan and Date.scan.
   *)
  val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
end

structure StringCvt :> STRING_CVT = struct end
