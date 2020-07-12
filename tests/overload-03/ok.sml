(* NOTE this is not strictly compatible with the Definition, because the declaration and usage of
 * `add` are different structure-level declarations, so we should have solved the type of `add`
 * as its default overload type, `int * int -> int`, when we reached the end of the structure-level
 * declaration that defined it.
 *)
val add = op+
signature S = sig end
val _ = add (1.1, 2.2)
