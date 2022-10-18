signature MLTON_CONT = sig
  (*!
   * the type of continuations that expect a value of type 'a.
   *)
  type 'a t
  (*!
   * callcc f applies f to the current continuation. This copies the entire stack; hence, callcc
   * takes time proportional to the size of the current stack.
   *)
  val callcc : ('a t -> 'a) -> 'a
  (*!
   * isolate f creates a continuation that evaluates f in an empty context. This is a constant time
   * operation, and yields a constant size stack.
   *)
  val isolate : ('a -> unit) -> 'a t
  (*!
   * prepend (k, f) composes a function f with a continuation k to create a continuation that first
   * does f and then does k. This is a constant time operation.
   *)
  val prepend : 'a t * ('b -> 'a) -> 'b t
  (*!
   * throw (k, v) is throws value v to continuation k. This copies the entire stack of k; hence,
   * throw takes time proportional to the size of this stack.
   *)
  val throw : 'a t * 'a -> 'b
  (*!
   * throw' (k, th) is a generalization of throw that evaluates th () in the context of k. Thus, for
   * example, if th () raises an exception or captures another continuation, it will see k, not the
   * current continuation.
   *)
  val throw' : 'a t * (unit -> 'a) -> 'b
end
