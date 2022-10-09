(*!
 * The built-in word type.
 *
 * Words are unsigned integers. They are written in decimal digits with the prefix `0w`.
 *
 * ```sml
 * val dec = 0w123
 * ```
 *
 * They can also be written in hex with the prefix `0wx`.
 *
 * ```sml
 * val hex = 0wx123beef
 * ```
 *
 * The `Word` structure provides operations on words.
 *
 * ```sml
 * val nine = Word.max (0w4, 0w9)
 * ```
 *)
type word = word
