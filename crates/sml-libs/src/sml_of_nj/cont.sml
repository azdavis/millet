signature CONT = sig
  (*!
  The type of continuations accepting arguments of type 'a
  !*)
  type 'a cont
  (*!
  callcc f Apply f to the "current continuation". If f invokes this continuation with argument x, it
  is as if (callcc f) had returned x as a result.
  !*)
  val callcc : ('a cont -> 'a) -> 'a
  (*!
  throw k a Invoke continuation k with argument a.
  !*)
  val throw : 'a cont -> 'a -> 'b
  (*!
  isolate f x Discard all live data from the calling context (except what is reachable from f or x),
  then call f(x), then exit. This may use much less memory then something like f(x) before exit().
  !*)
  val isolate : ('a -> unit) -> 'a cont
  (*!
  The type of "primitive" continuations accepting arguments of type 'a. Unlike an ordinary
  continuation cont, a control_cont does not remember the exception-handler of its creator.
  !*)
  type 'a control_cont
  (*!
  capture f Apply f to the "current continuation". If f invokes this continuation with argument x,
  it is as if (capture f) had returned x as a result, except that the exception-handler is not
  properly restored (it is still that of the invoker).
  !*)
  val capture : ('a control_cont -> 'a) -> 'a
  (*!
  escape k a Invoke primitive continuation k with argument a, which is like returning from the
  original capture except that the exception handler is that of whoever called escape k a.
  !*)
  val escape : 'a control_cont -> 'a -> 'b
end

structure Cont : CONT = struct end
