signature POLL = sig
  exception BadPollFreq
  val pollEvent : bool ref
  val setHandler : (unit Cont.cont -> unit Cont.cont) option -> unit
  val inqHandler : unit -> (unit Cont.cont -> unit Cont.cont) option
  val setFreq : int option -> unit
  val inqFreq : unit -> int option
end
