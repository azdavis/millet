(*!
 * The built-in integer type.
 *
 * Integers may be positive, negative, or 0. They are usually written with decimal digits.
 *
 * ```sml
 * val pos = 4
 * val neg = ~8
 * val zero = 0
 * ```
 *
 * They can also be written in hex, with the prefix `0x`.
 *
 * ```sml
 * val hex = 0x123beef
 * ```
 *
 * The `Int` structure provides operations on integers.
 *
 * ```sml
 * val three = Int.min (3, 5)
 * ```
 *)
type int = int
