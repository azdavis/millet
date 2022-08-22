structure String :> sig type string = string end = struct end
structure CharVector :> sig type vector end = struct end
structure Char :> sig type char = char end = struct end
structure WideCharVector :> sig type vector end = struct end
structure WideChar :> sig type char end = struct end

(*!
 * The STRING signature specifies the basic operations on a string type, which is a vector of the
 * underlying character type char as defined in the structure.
 *
 * The STRING signature is matched by two structures, the required String and the optional
 * WideString. The former implements strings based on the extended ASCII 8-bit characters, and is a
 * companion structure to the Char structure. The latter provides strings of characters of some size
 * greater than or equal to 8 bits, and is related to the structure WideChar. In particular, the
 * type String.char is identical to the type Char.char and, when WideString is defined, the type
 * WideString.char is identical to the type WideChar.char. These connections are made explicit in
 * the Text and WideText structures, which match the TEXT signature.
 *)
signature STRING = sig
  eqtype string
  eqtype char
  (*!
   * The longest allowed size of a string.
   *)
  val maxSize : int
  (*!
   * size s returns |s|, the number of characters in string s.
   *)
  val size : string -> int
  (*!
   * sub (s, i) returns the i(th) character of s, counting from zero. This raises Subscript if i < 0
   * or |s| <= i.
   *)
  val sub : string * int -> char
  (*!
   * See extract.
   *)
  val extract : string * int * int option -> string
  (*!
   * See extract.
   *)
  val substring : string * int * int -> string
  (*!
   * s ^ t is the concatenation of the strings s and t. This raises Size if |s| + |t| > maxSize.
   *)
  val ^ : string * string -> string
  (*!
   * concat l is the concatenation of all the strings in l. This raises Size if the sum of all the
   * sizes is greater than maxSize.
   *)
  val concat : string list -> string
  (*!
   * concatWith s l returns the concatenation of the strings in the list l using the string s as a
   * separator. This raises Size if the size of the resulting string would be greater than maxSize.
   *)
  val concatWith : string -> string list -> string
  (*!
   * str c is the string of size one containing the character c.
   *)
  val str : char -> string
  (*!
   * implode l generates the string containing the characters in the list l. This is equivalent to
   * concat (List.map str l). This raises Size if the resulting string would have size greater than
   * maxSize.
   *)
  val implode : char list -> string
  (*!
   * explode s is the list of characters in the string s.
   *)
  val explode : string -> char list
  (*!
   * map f s applies f to each element of s from left to right, returning the resulting string. It
   * is equivalent to implode(List.map f (explode s)).
   *)
  val map : (char -> char) -> string -> string
  (*!
   * translate f s returns the string generated from s by mapping each character in s by f. It is
   * equivalent to concat(List.map f (explode s)).
   *)
  val translate : (char -> string) -> string -> string
  (*!
   * These functions return a list of tokens or fields, respectively, derived from s from left to
   * right. A token is a non-empty maximal substring of s not containing any delimiter. A field is a
   * (possibly empty) maximal substring of s not containing any delimiter. In both cases, a
   * delimiter is a character satisfying the predicate f. Two tokens may be separated by more than
   * one delimiter, whereas two fields are separated by exactly one delimiter. For example, if the
   * only delimiter is the character #"|", then the string "|abc||def" contains two tokens "abc" and
   * "def", whereas it contains the four fields "", "abc", "" and "def".
   *)
  val tokens : (char -> bool) -> string -> string list
  (*!
   * See tokens.
   *)
  val fields : (char -> bool) -> string -> string list
  (*!
   * These functions return true if the string s1 is a prefix, substring, or suffix (respectively)
   * of the string s2. Note that the empty string is a prefix, substring, and suffix of any string,
   * and that a string is a prefix, substring, and suffix of itself.
   *)
  val isPrefix : string -> string -> bool
  (*!
   * See isPrefix.
   *)
  val isSubstring : string -> string -> bool
  (*!
   * See isPrefix.
   *)
  val isSuffix : string -> string -> bool
  (*!
   * compare (s, t) does a lexicographic comparison of the two strings using the ordering
   * Char.compare on the characters. It returns LESS, EQUAL, or GREATER, if s is less than, equal
   * to, or greater than t, respectively.
   *)
  val compare : string * string -> order
  (*!
   * collate f (s, t) performs lexicographic comparison of the two strings using the given ordering
   * f on characters.
   *)
  val collate : (char * char -> order) -> string * string -> order
  (*!
   * These functions compare two strings lexicographically, using the underlying ordering on the
   * char type.
   *)
  val < : string * string -> bool
  val <= : string * string -> bool
  val > : string * string -> bool
  val >= : string * string -> bool
  (*!
   * toString s returns a string corresponding to s, with non-printable characters replaced by SML
   * escape sequences. This is equivalent to translate Char.toString s
   *)
  val toString : string -> String.string
  (*!
   * These functions scan their character source as a sequence of printable characters, converting
   * SML escape sequences into the appropriate characters. They do not skip leading whitespace. They
   * return as many characters as can successfully be scanned, stopping when they reach the end of
   * the source or a non-printing character (i.e., one not satisfying isPrint), or if they encounter
   * an improper escape sequence. fromString ignores the remaining characters, while scan returns
   * the remaining characters as the rest of the stream. The function fromString is equivalent to
   * the StringCvt.scanString scan. If no conversion is possible, e.g., if the first character is
   * non-printable or begins an illegal escape sequence, NONE is returned. Note, however, that
   * fromString "" returns SOME(""). For more information on the allowed escape sequences, see the
   * entry for CHAR.fromString. SML source also allows escaped formatting sequences, which are
   * ignored during conversion. The rule is that if any prefix of the input is successfully scanned,
   * including an escaped formatting sequence, the functions returns some string. They only return
   * NONE in the case where the prefix of the input cannot be scanned at all. Here are some sample
   * conversions: Input string s fromString s "\\q" NONE "a\^D" SOME "a" "a\\ \\\\q" SOME "a" "\\
   * \\" SOME "" "" SOME "" "\\ \\\^D" SOME "" "\\ a" NONE Implementation note: Because of the
   * special cases, such as fromString "" = SOME "", fromString "\\ \\\^D" = SOME "", and fromString
   * "\^D" = NONE, the functions cannot be implemented as a simple iterative application of
   * CHAR.scan.
   *)
  val scan : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
  (*!
   * See scan.
   *)
  val fromString : String.string -> string option
  (*!
   * toCString s returns a string corresponding to s, with non-printable characters replaced by C
   * escape sequences. This is equivalent to translate Char.toCString s
   *)
  val toCString : string -> String.string
  (*!
   * fromCString s scans the string s as a string in the C language, converting C escape sequences
   * into the appropriate characters. The semantics are identical to fromString above, except that C
   * escape sequences are used (see ISO C standard ISO/IEC 9899:1990[CITE]). For more information on
   * the allowed escape sequences, see the entry for CHAR.fromCString. Note that fromCString accepts
   * an unescaped single quote character, but does not accept an unescaped double quote character.
   *)
  val fromCString : String.string -> string option
end

structure String :> STRING
  where type string = string
  where type char = Char.char = String
structure WideString :> STRING (* OPTIONAL *)
  where type string = WideCharVector.vector
  where type char = WideChar.char = struct end

val op ^ = String.^
val explode = String.explode
val implode = String.implode
val size = String.size
val str = String.str
