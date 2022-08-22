(*!
 * The Option structure defines the option type, used for handling partial functions and optional
 * values, and provides a collection of common combinators.
 *
 * The type, the Option exception, and the functions getOpt, valOf, and isSome are available in the
 * top-level environment.
 *)
signature OPTION = sig
  (*!
   * The type option provides a distinction between some value and no value, and is often used for
   * representing the result of partially defined functions. It can be viewed as a typed version of
   * the C convention of returning a NULL pointer to indicate no value.
   *)
  datatype 'a option = NONE | SOME of 'a
  exception Option
  (*!
   * getOpt (opt, a) returns v if opt is SOME(v); otherwise it returns a.
   *)
  val getOpt : 'a option * 'a -> 'a
  (*!
   * isSome opt returns true if opt is SOME(v); otherwise it returns false.
   *)
  val isSome : 'a option -> bool
  (* non-standard *)
  val isNone : 'a option -> bool
  (*!
   * valOf opt returns v if opt is SOME(v); otherwise it raises the Option exception.
   *)
  val valOf : 'a option -> 'a
  (*!
   * filter f a returns SOME(a) if f(a) is true and NONE otherwise.
   *)
  val filter : ('a -> bool) -> 'a -> 'a option
  (*!
   * The join function maps NONE to NONE and SOME(v) to v.
   *)
  val join : 'a option option -> 'a option
  (*!
   * app f opt applies the function f to the value v if opt is SOME(v), and otherwise does nothing.
   *)
  val app : ('a -> unit) -> 'a option -> unit
  (*!
   * map f opt maps NONE to NONE and SOME(v) to SOME(f v).
   *)
  val map : ('a -> 'b) -> 'a option -> 'b option
  (*!
   * mapPartial f opt maps NONE to NONE and SOME(v) to f(v). The expression mapPartial f is
   * equivalent to join o (map f).
   *)
  val mapPartial : ('a -> 'b option) -> 'a option -> 'b option
  (*!
   * compose (f, g) a returns NONE if g(a) is NONE; otherwise, if g(a) is SOME(v), it returns SOME(f
   * v). Thus, the compose function composes f with the partial function g to produce another
   * partial function. The expression compose (f, g) is equivalent to (map f) o g.
   *)
  val compose : ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option
  (*!
   * composePartial (f, g) a returns NONE if g(a) is NONE; otherwise, if g(a) is SOME(v), it returns
   * f(v). Thus, the composePartial function composes the two partial functions f and g to produce
   * another partial function. The expression composePartial (f, g) is equivalent to (mapPartial f)
   * o g.
   *)
  val composePartial : ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option
end

structure Option :> OPTION = struct end

datatype option = datatype Option.option
exception Option = Option.Option
val getOpt = Option.getOpt
val isSome = Option.isSome
val valOf = Option.valOf
