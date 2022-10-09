(*!
 * The built-in string type.
 *
 * Literals are delimited with double quotes, `"`.
 *
 * ```sml
 * val message = "hello, world!"
 * ```
 *
 * There are various string escapes available, all of which start with `\`.
 *
 * ```sml
 * val () = print "hi\n"
 * ```
 *
 * Strings can be concatenated with `^`.
 *
 * ```sml
 * val foobar = "foo" ^ "bar"
 * ```
 *
 * The `String` structure provides operations on strings.
 *
 * ```sml
 * val yes = String.isSubstring "erica" "america"
 * ```
 *
 * Strings are often thought of as ordered sequences of characters. Indeed, in SML, there is
 * `String.explode` and `String.implode` to go from strings to list of characters and vice versa.
 * However, the notion of "character" is [difficult to define][1].
 *
 * [1]: https://home.unicode.org
 *)
type string = string
