(*!
 * The built-in character type.
 *
 * Literals are written like string literals (delimited with `"`), except they must:
 *
 * - Have length 1.
 * - Have a leading `#`.
 *
 * ```sml
 * val percent = #"%"
 * val space = #" "
 * ```
 *
 * Millet, and SML in general, has limited support for extended character sets beyond ASCII.
 *
 * The `Char` structure provides operations on characters.
 *
 * ```sml
 * val no = Char.isSpace #"g"
 * ```
 *)
type char = char
