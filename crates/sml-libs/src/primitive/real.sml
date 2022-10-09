(*!
 * The built-in real number type.
 *
 * This type approximates the "mathematical" real numbers by using [floating-point arithmetic][1].
 *
 * ```sml
 * val r = 1.4
 * val closeToZeroPoint3 = 0.1 + 0.2
 * ```
 *
 * Literals can have an exponent with an `e` or `E` following the base.
 *
 * ```sml
 * val withExp = 1.4e5
 * ```
 *
 * The `Real` structure provides operations on reals.
 *
 * ```sml
 * val four = Real.sqrt 16.0
 * ```
 *
 * [1]: https://0.30000000000000004.com
 *)
type real = real
