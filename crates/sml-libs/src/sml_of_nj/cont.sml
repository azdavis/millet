signature CONT = sig
  type 'a cont
  val callcc : ('a cont -> 'a) -> 'a
  val throw : 'a cont -> 'a -> 'b
  val isolate : ('a -> unit) -> 'a cont
  type 'a control_cont
  val capture : ('a control_cont -> 'a) -> 'a
  val escape : 'a control_cont -> 'a -> 'b
end

structure Cont : CONT = struct end
