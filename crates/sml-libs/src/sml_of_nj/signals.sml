signature SIGNALS = sig
  eqtype signal
  datatype sig_action = IGNORE | DEFAULT | HANDLER of (signal * int * unit Cont.cont) -> unit Cont.cont
  val listSignals : unit -> signal list
  val toString : signal -> string
  val fromString : string -> signal option
  val setHandler : (signal * sig_action) -> sig_action
  val overrideHandler : (signal * sig_action) -> sig_action
  val inqHandler : signal -> sig_action
  datatype sigmask = MASKALL | MASK of signal list
  val maskSignals : sigmask -> unit
  val unmaskSignals : sigmask -> unit
  val masked : unit -> sigmask
  val pause : unit -> unit
  val sigINT : signal
  val sigALRM : signal
  val sigTERM : signal
  val sigGC : signal
end

structure Signals : SIGNALS = struct end
