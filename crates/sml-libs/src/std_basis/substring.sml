structure CharVectorSlice :> sig type slice end = struct end
structure WideCharVectorSlice :> sig type slice end = struct end

(*!
 * The SUBSTRING signature specifies manipulations on an abstract representation of a sequence of
 * contiguous characters in a string. A substring value can be modeled as a triple (s, i, n), where
 * s is the underlying string, i is the starting index, and n is the size of the substring, with the
 * constraint that 0 <= i <= i + n <= |s|.
 *
 * The substring type and its attendant functions provide a convenient abstraction for performing a
 * variety of common analyses of strings, such as finding the leftmost occurrence, if any, of a
 * character in a string. In addition, using the substring functions avoids much of the copying and
 * bounds checking that occur if similar operations are implemented solely in terms of strings.
 *
 * The SUBSTRING signature is matched by two structures, the required Substring and the optional
 * WideSubstring. The former is a companion structure to the Char and String structures, which are
 * based on the extended ASCII 8-bit character set. The structure WideSubstring is related in the
 * same way to the structures WideChar and WideString, which are based on characters of some size
 * greater than or equal to 8 bits. In particular, the types Substring.string and Substring.char are
 * identical to those types in the structure String and, when WideSubstring is defined, the types
 * WideSubstring.string and WideSubstring.char are identical to those types in the structure
 * WideString.
 *
 * All of these connections are made explicit in the Text and WideText structures, which match the
 * TEXT signature. In the exposition below, references to a String structure refers to the
 * substructure of that name defined in either the Text or the WideText structure, which ever is
 * appropriate.
 *
 * The design of the SUBSTRING interface was influenced by the paper ``Subsequence References:
 * First-Class Values for Substrings,'' by Wilfred J. Hansen[CITE].
 *)
signature SUBSTRING = sig
  type substring
  eqtype char
  eqtype string
  (*!
   * sub (s, i) returns the i(th) character in the substring, counting from the beginning of s. It
   * is equivalent to String.sub(string s, i). The exception Subscript is raised unless 0 <= i <
   * |s|.
   *)
  val sub : substring * int -> char
  (*!
   * size s returns the size of s. This is equivalent to #3 o base and String.size o string.
   *)
  val size : substring -> int
  (*!
   * base ss returns a triple (s, i, n) giving a concrete representation of the substring. s is the
   * underlying string, i is the starting index, and n is the size of the substring. It will always
   * be the case that 0 <= i <= i + n <= |s| .
   *)
  val base : substring -> string * int * int
  (*!
   * See extract.
   *)
  val extract : string * int * int option -> substring
  (*!
   * See extract.
   *)
  val substring : string * int * int -> substring
  (*!
   * full s creates a substring representing the entire string s. It is equivalent to the expression
   * substring(s, 0, String.size s).
   *)
  val full : string -> substring
  (*!
   * string s creates a string value corresponding to the substring. It is equivalent to
   * String.substring o base for the corresponding String structure.
   *)
  val string : substring -> string
  (*!
   * isEmpty s returns true if s has size 0.
   *)
  val isEmpty : substring -> bool
  (*!
   * getc s returns the first character in s and the rest of the substring, or NONE if s is empty.
   *)
  val getc : substring -> (char * substring) option
  (*!
   * first s returns the first character in s, or NONE if s is empty.
   *)
  val first : substring -> char option
  (*!
   * These functions remove k characters from the left (respectively, right) of the substring s. If
   * k is greater than the size of the substring, an empty substring is returned. Specifically, for
   * substring ss = substring(s, i, j) and k <= j, we have: triml k ss = substring(s, i+k, j-k)
   * trimr k ss = substring(s, i, j-k) The exception Subscript is raised if k < 0. This exception is
   * raised when triml k or trimr k is evaluated.
   *)
  val triml : int -> substring -> substring
  (*!
   * See triml.
   *)
  val trimr : int -> substring -> substring
  (*!
   * See slice.
   *)
  val slice : substring * int * int option -> substring
  (*!
   * concat l generates a string that is the concatenation of the substrings in l. This is
   * equivalent to String.concat o (List.map string). This raises Size if the sum of all the sizes
   * is greater than the corresponding maxSize for the string type.
   *)
  val concat : substring list -> string
  (*!
   * concatWith s l returns the concatenation of the substrings in the list l using the string s as
   * a separator. This raises Size if the size of the resulting string would be greater than maxSize
   * for the string type.
   *)
  val concatWith : string -> substring list -> string
  (*!
   * explode s returns the list of characters composing the substring. This is equivalent to
   * String.explode (string s).
   *)
  val explode : substring -> char list
  (*!
   * These functions return true if the string s is a prefix, substring, or suffix (respectively) of
   * the substring ss. The functions are equivalent to their versions from STRING. For example,
   * isPrefix s ss is the same as String.isPrefix s (string ss).
   *)
  val isPrefix : string -> substring -> bool
  (*!
   * See isPrefix.
   *)
  val isSubstring : string -> substring -> bool
  (*!
   * See isPrefix.
   *)
  val isSuffix : string -> substring -> bool
  (*!
   * compare (s, t) compares the two substrings lexicographically using the default character
   * comparison function. This is equivalent to String.compare (string s, string t)
   *)
  val compare : substring * substring -> order
  (*!
   * collate f (s, t) compares the two substrings lexicographically using the character comparison
   * function f. This is equivalent to String.collate f (string s, string t)
   *)
  val collate : (char * char -> order) -> substring * substring -> order
  (*!
   * These functions scan s from left to right (respectively, right to left) looking for the first
   * character that does not satisfy the predicate f. They return the pair (ls, rs) giving the split
   * of the substring into the span up to that character and the rest. ls is the left side of the
   * split, and rs is the right side. For example, if the characters a and c satisfy the predicate,
   * but character X does not, then these functions work as follows on the substring aaaXbbbbXccc:
   * splitl : aaa XbbbbXccc splitr : aaaXbbbbX ccc
   *)
  val splitl : (char -> bool) -> substring -> substring * substring
  (*!
   * See splitl.
   *)
  val splitr : (char -> bool) -> substring -> substring * substring
  (*!
   * splitAt (s, i) returns the pair of substring (ss, ss'), where ss contains the first i
   * characters of s and ss' contains the rest, assuming 0 <= i <= size s. Otherwise, it raises
   * Subscript.
   *)
  val splitAt : substring * int -> substring * substring
  (*!
   * These routines scan the substring s for the first character not satisfying the predicate p. The
   * functions dropl and takel scan left to right (i.e., increasing character indices), while dropr
   * and taker scan from the right. The drop functions drop the maximal substring consisting of
   * characters satisfying the predicate, while the take functions return the maximal such
   * substring. These can be defined in terms of the split operations: takel p s = #1(splitl p s)
   * dropl p s = #2(splitl p s) taker p s = #2(splitr p s) dropr p s = #1(splitr p s)
   *)
  val dropl : (char -> bool) -> substring -> substring
  (*!
   * See dropl.
   *)
  val dropr : (char -> bool) -> substring -> substring
  (*!
   * See dropl.
   *)
  val takel : (char -> bool) -> substring -> substring
  (*!
   * See dropl.
   *)
  val taker : (char -> bool) -> substring -> substring
  (*!
   * position s ss splits the substring ss into a pair (pref, suff) of substrings, where suff is the
   * longest suffix of ss that has s as a prefix and pref is the prefix of ss preceding suff. More
   * precisely, let m be the size of s and let ss correspond to the substring (s', i, n). If there
   * is a least index k >= i such that s = s'[k..k+m-1], then suff corresponds to (s', k, n+i-k) and
   * pref corresponds to (s', i, k-i). If there is no such k, then suff is the empty substring
   * corresponding to (s', i+n, 0) and pref corresponds to (s', i, n), i.e., all of ss.
   *)
  val position : string -> substring -> substring * substring
  (*!
   * span (ss, ss') produces a substring composed of a prefix ss, suffix ss', plus all intermediate
   * characters in the underlying string. It raises Span if ss and ss' are not substrings of the
   * same underlying string or if the start of ss is to the right of the end of ss'. More precisely,
   * if we have val (s, i, n) = base ss val (s', i', n') = base ss' then span returns substring(s,
   * i, (i'+n')-i) unless s <> s' or i'+n' < i, in which case it raises Span. Note that this does
   * not preclude ss' from beginning to the left of ss, or ss from ending to the right of ss'. This
   * function allows one to scan for a substring using multiple pieces and then coalescing the
   * pieces. For example, given a URL string such as "http://www.standardml.org/Basis/overview.html"
   * to scan the protocol and host ("http://www.standardml.org"), one could write: local open
   * Substring in fun protoAndHost url = let fun notc (c : char) = fn c' => c <> c' val (proto,rest)
   * = splitl (notc #":") (full url) val host = takel (notc #"/") (triml 3 rest) in span (proto,
   * host) end end Implementation note: When applied to substrings derived from the identical base
   * string, the string equality test should be constant time. This can be achieved by first doing a
   * pointer test and, only if that fails, then checking the strings character by character.
   *)
  val span : substring * substring -> substring
  (*!
   * translate f s applies f to every character of s, from left to right, and returns the
   * concatenation of the results. This is equivalent to String.concat(List.map f (explode s)).
   *)
  val translate : (char -> string) -> substring -> string
  (*!
   * These functions decompose a substring into a list of tokens or fields from left to right. A
   * token is a non-empty maximal substring not containing any delimiter. A field is a (possibly
   * empty) maximal substring of s not containing any delimiter. In both cases, a delimiter is a
   * character satisfying predicate f. Two tokens may be separated by more than one delimiter,
   * whereas two fields are separated by exactly one delimiter. For example, if the only delimiter
   * is the character #"|", then the substring "|abc||def" contains two tokens "abc" and "def",
   * whereas it contains the four fields "", "abc", "" and "def".
   *)
  val tokens : (char -> bool) -> substring -> substring list
  (*!
   * See tokens.
   *)
  val fields : (char -> bool) -> substring -> substring list
  (*!
   * app f s applies f to each character of s from left to right. It is equivalent to List.app f
   * (explode s).
   *)
  val app : (char -> unit) -> substring -> unit
  (*!
   * These fold the function f over the substring s, starting with the value a, from left to right
   * and from right to left, respectively. They are the analogues of the identically named functions
   * in List. In particular, they are respectively equivalent to: List.foldl f a (explode s)
   * List.foldr f a (explode s)
   *)
  val foldl : (char * 'a -> 'a) -> 'a -> substring -> 'a
  (*!
   * See foldl.
   *)
  val foldr : (char * 'a -> 'a) -> 'a -> substring -> 'a
end

structure Substring :> SUBSTRING
  where type substring = CharVectorSlice.slice
  where type string = String.string
  where type char = Char.char = struct end

structure WideSubstring :> SUBSTRING (* OPTIONAL *)
  where type substring = WideCharVectorSlice.slice
  where type string = WideString.string
  where type char = WideChar.char = struct end

(*
 * Alias for `Substring.substring`.
 *)
datatype substring = datatype Substring.substring
(*
 * Alias for `Substring.substring`.
 *)
val substring = Substring.substring
