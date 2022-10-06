(*!
Signals provides an interface to signal system.
!*)
signature SIGNALS = sig
  (*!
  The type of signals (HUP, INT, QUIT, ...) deliverable to a process, plus ML-specific
  pseudo-signals such as "GC".
  !*)
  eqtype signal
  datatype sig_action = IGNORE | DEFAULT | HANDLER of (signal * int * unit Cont.cont) -> unit Cont.cont
  (*!
  listSignals () Return a list of all signals recognized by the signal system. Since signal is an
  abstract type, one may wish to examine map toString (listSignals()) to see their names.
  !*)
  val listSignals : unit -> signal list
  (*!
  toString signal Get the name of signal. Example: "HUP", "INT", "QUIT", etc.
  !*)
  val toString : signal -> string
  (*!
  fromString s Convert a string to a signal, or return NONE if there is no such signal. The
  structure UnixSignals defines several of these signal values, so that you need not suffer the
  indignity and insecurity of using fromString to look them up.
  !*)
  val fromString : string -> signal option
  (*!
  See setHandler.
  !*)
  val setHandler : (signal * sig_action) -> sig_action
  (*!
  overrideHandler (signal, handler) If signal is not being ignored, then set the handler. This
  returns the previous handler (if IGNORE, then the current handler is still IGNORE).
  !*)
  val overrideHandler : (signal * sig_action) -> sig_action
  (*!
  inqHandler signal Get the current action for signal.
  !*)
  val inqHandler : signal -> sig_action
  datatype sigmask = MASKALL | MASK of signal list
  (*!
  maskSignals sigs Mask the specified set of signals: signals that are not IGNORED will be delivered
  when unmasked. Calls to maskSignals nest on a per signal basis.
  !*)
  val maskSignals : sigmask -> unit
  (*!
  unmaskSignals sigs Unmask the specified signals. The unmasking of a signal that is not masked has
  no effect.
  !*)
  val unmaskSignals : sigmask -> unit
  (*!
  masked () Return the set of masked signals; the value MASK[] means that no signals are masked.
  !*)
  val masked : unit -> sigmask
  (*!
  pause () Sleep until the next signal; if called when signals are masked, then signals will still
  be masked when pause returns.
  !*)
  val pause : unit -> unit
  (*!
  sigINT The interactive interrupt.
  !*)
  val sigINT : signal
  (*!
  sigALRM The interval timer signal.
  !*)
  val sigALRM : signal
  (*!
  sigTERM Process termination.
  !*)
  val sigTERM : signal
  (*!
  sigGC Signalled by the runtime system (not by the operating system) immediately after each garbage
  collection.
  !*)
  val sigGC : signal
end

structure Signals : SIGNALS = struct end
