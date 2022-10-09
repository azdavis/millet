(*!
 * The built-in boolean type.
 *
 * There are two boolean values: `true` and `false`.
 *
 * ```sml
 * val yes = true
 * val no = false
 * ```
 *
 * Functions that check for equality or perform comparisions, like `=` and `>`, often return `bool`.
 *
 * ```sml
 * val yep = "foo" ^ "bar" = "foobar"
 * val nope = 3 > 7
 * ```
 *
 * `if` expressions case on the value of a `bool`.
 *
 * ```sml
 * fun choose (b : bool) = if b then "yea" else "nah"
 * ```
 *
 * The `Bool` structure provides operations on booleans.
 *
 * ```sml
 * val s = Bool.toString false
 * ```
 *)
type bool = bool
