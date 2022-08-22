structure Word :> sig type word = word end = struct end
structure LargeWord :> sig type word end = struct end

(*!
 * Instances of the signature WORD provide a type of unsigned integer with modular arithmetic and
 * logical operations and conversion operations. They are also meant to give efficient access to the
 * primitive machine word types of the underlying hardware, and support bit-level operations on
 * integers. They are not meant to be a ``larger'' int.
 *
 * In order to provide a more intuitive description of the shift operators below, we assume a bit
 * ordering in which the most significant bit is leftmost, and the least significant bit is
 * rightmost.
 *)
signature WORD = sig
  eqtype word
  (*!
   * The number of bits in type word. wordSize need not be a power of two. Note that word has a
   * fixed, finite precision.
   *)
  val wordSize : int
  (*!
   * These convert w to a value of type LargeWord.word. In the first case, w is converted to its
   * equivalent LargeWord.word value in the range [0,2(wordSize)-1]. In the second case, w is
   * ``sign-extended,'' i.e., the wordSize low-order bits of w and toLargeX w are the same, and the
   * remaining bits of toLargeX w are all equal to the most significant bit of w. toLargeWord and
   * toLargeWordX are respective synonyms of the first two, and are deprecated.
   *)
  val toLarge : word -> LargeWord.word
  (*!
   * See toLarge.
   *)
  val toLargeX : word -> LargeWord.word
  val toLargeWord : word -> LargeWord.word
  val toLargeWordX : word -> LargeWord.word
  (*!
   * These functions convert w to the value w(mod (2(wordSize))) of type word. This has the effect
   * of taking the low-order wordSize bits of the 2's complement representation of w. fromLargeWord
   * is a deprecated synonym for fromLarge.
   *)
  val fromLarge : LargeWord.word -> word
  (*!
   * See fromLarge.
   *)
  val fromLargeWord : LargeWord.word -> word
  (*!
   * These convert w to a value of type LargeInt.int. In the former case, w is viewed as an integer
   * value in the range [0,2(wordSize)-1]. In the latter case, w is treated as a 2's complement
   * signed integer with wordSize precision, thereby having a value in the range
   * [-2(wordSize-1),2(wordSize-1)-1]. toLargeInt raises Overflow if the target integer value cannot
   * be represented as a LargeInt.int. Since the precision of LargeInt.int is always at least
   * wordSize (see the discussion below), toLargeIntX will never raise an exception.
   *)
  val toLargeInt : word -> LargeInt.int
  (*!
   * See toLargeInt.
   *)
  val toLargeIntX : word -> LargeInt.int
  (*!
   * fromLargeInt i converts i of type LargeInt.int to a value of type word. This has the effect of
   * taking the low-order wordSize bits of the 2's complement representation of i.
   *)
  val fromLargeInt : LargeInt.int -> word
  (*!
   * These convert w to a value of default integer type. In the former case, w is viewed as an
   * integer value in the range [0,2(wordSize)-1]. In the latter case, w is treated as a 2's
   * complement signed integer with wordSize precision, thereby having a value in the range
   * [-2(wordSize-1),2(wordSize-1)-1]. They raise Overflow if the target integer value cannot be
   * represented as an Int.int.
   *)
  val toInt : word -> int
  (*!
   * See toInt.
   *)
  val toIntX : word -> int
  (*!
   * fromInt i converts i of the default integer type to a value of type word. This has the effect
   * of taking the low-order wordSize bits of the 2's complement representation of i. If the
   * precision of Int.int is less than wordSize, then i is sign-extended to wordSize bits.
   *)
  val fromInt : int -> word
  (*!
   * These functions return the bit-wise AND, OR, and exclusive OR, respectively, of their
   * arguments.
   *)
  val andb : word * word -> word
  val orb : word * word -> word
  val xorb : word * word -> word
  (*!
   * notb i returns the bit-wise complement (NOT) of i.
   *)
  val notb : word -> word
  (*!
   * << (i, n) shifts i to the left by n bit positions, filling in zeros from the right. When i and
   * n are interpreted as unsigned binary numbers, this returns (i* 2(n))(mod (2 (wordSize))). In
   * particular, shifting by greater than or equal to wordSize results in 0. This operation is
   * similar to the ``(logical) shift left'' instruction in many processors.
   *)
  val << : word * Word.word -> word
  (*!
   * >> (i, n) shifts i to the right by n bit positions, filling in zeros from the left. When i and
   * n are interpreted as unsigned binary numbers, it returns floor((i / 2(n))). In particular,
   * shifting by greater than or equal to wordSize results in 0. This operation is similar to the
   * ``logical shift right'' instruction in many processors.
   *)
  val >> : word * Word.word -> word
  (*!
   * ~>> (i, n) shifts i to the right by n bit positions. The value of the leftmost bit of i remains
   * the same; in a 2's-complement interpretation, this corresponds to sign extension. When i is
   * interpreted as a wordSize-bit 2's-complement integer and n is interpreted as an unsigned binary
   * number, it returns floor((i / 2(n))). In particular, shifting by greater than or equal to
   * wordSize results in either 0 or all 1's. This operation is similar to the ``arithmetic shift
   * right'' instruction in many processors.
   *)
  val ~>> : word * Word.word -> word
  val + : word * word -> word
  val - : word * word -> word
  val * : word * word -> word
  (*!
   * i div j returns the truncated quotient of i and j, floor((i / j)), when i and j are interpreted
   * as unsigned binary numbers. It raises Div when j = 0.
   *)
  val div : word * word -> word
  (*!
   * i mod j returns the remainder of the division of i by j: i - j * floor((i / j)) when i and j
   * are interpreted as unsigned binary numbers. It raises Div when j = 0.
   *)
  val mod : word * word -> word
  (*!
   * compare (i, j) returns LESS, EQUAL, or GREATER if and only if i is less than, equal to, or
   * greater than j, respectively, considered as unsigned binary numbers.
   *)
  val compare : word * word -> order
  (*!
   * These return true if and only if the input arguments satisfy the given relation when
   * interpreted as unsigned binary numbers.
   *)
  val < : word * word -> bool
  val <= : word * word -> bool
  val > : word * word -> bool
  val >= : word * word -> bool
  (*!
   * ~ i returns the 2's complement of i.
   *)
  val ~ : word -> word
  (*!
   * These return the smaller (respectively, larger) of the arguments.
   *)
  val min : word * word -> word
  val max : word * word -> word
  (*!
   * These return a string containing a numeric representation of i. No prefix "Ow", "OwX", etc. is
   * generated. The version using fmt creates a representation specified the given radix. The
   * hexadecimal digits in the range [10,15] are represented by the characters #"A" through #"F".
   * The version using toString is equivalent to fmt StringCvt.HEX i.
   *)
  val fmt : StringCvt.radix -> word -> string
  (*!
   * See fmt.
   *)
  val toString : word -> string
  (*!
   * These functions scan a word from a character source. In the first version, if an unsigned
   * number in the format denoted by radix can be parsed from a prefix of the character strm strm
   * using the character input function getc, the expression evaluates to SOME(w,rest), where w is
   * the value of the number parsed and rest is the remainder of the character stream. Initial
   * whitespace is ignored. NONE is returned otherwise. It raises Overflow when a number can be
   * parsed, but is too large to fit in type word. The format that scan accepts depends on the radix
   * argument. Regular expressions defining these formats are as follows: Radix Format StringCvt.BIN
   * (0w)?[0-1]+ StringCvt.OCT (0w)?[0-7]+ StringCvt.DEC (0w)?[0-9]+ StringCvt.HEX (0wx | 0wX | 0x |
   * 0X)?[0-9a-fA-F]+ The fromString version returns SOME(w) if an unsigned hexadecimal number in
   * the format (0wx | 0wX | 0x | 0X)?[0-9a-fA-F]+ can be parsed from a prefix of string s, ignoring
   * initial whitespace, where w is the value of the number parsed. NONE is returned otherwise. This
   * function raises Overflow when a hexadecimal numeral can be parsed, but is too large to be
   * represented by type word. It is equivalent to StringCvt.scanString (scan StringCvt.HEX)
   *)
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
  (*!
   * See scan.
   *)
  val fromString : string -> word option
end

structure Word :> WORD
  where type word = word = Word
structure Word8 :> WORD = struct end
structure Word16 :> WORD (* OPTIONAL *) = struct end
structure Word31 :> WORD (* OPTIONAL *) = struct end
structure LargeWord :> WORD
  where type word = LargeWord.word = LargeWord
structure Word32 (* OPTIONAL *) = Word
structure Word64 (* OPTIONAL *) = LargeWord
structure SysWord :> WORD (* OPTIONAL *) = struct end
