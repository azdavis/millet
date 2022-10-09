(*!
 * The built-in reference type.
 *
 * This is the core primitive for imperative programming in SML. It is one of the few types in SML
 * whose values are mutable.
 *
 * - To construct a ref, use `ref`.
 * - To get the current value in a ref, use `!`.
 * - To set the value in a ref to a new value, use `:=`.
 *
 * ```sml
 * val r = ref 0
 * val zero = !r
 * val () = r := 3
 * val three = !r
 * ```
 *
 * The `Ref` structure provides operations on references.
 *
 * ```sml
 * val r1 = ref 3
 * val r2 = ref 8
 * val () = Ref.swap (r1, r2)
 * val eight = !r1
 * val three = !r2
 * ```
 *)
type 'a ref = 'a ref
