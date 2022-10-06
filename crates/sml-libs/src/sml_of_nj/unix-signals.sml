(*!
This structure defines several common Unix signal values, so that you need not suffer the indignity
and insecurity of using Signals.fromString to look them up.
!*)
signature UNIX_SIGNALS = sig
  include SIGNALS
    where type signal = Signals.signal
  val sigPIPE : signal
  val sigQUIT : signal
  val sigUSR1 : signal
  val sigUSR2 : signal
  val sigCHLD : signal
  val sigCONT : signal
  val sigTSTP : signal
  val sigTTIN : signal
  val sigTTOU : signal
end

structure UnixSignals : UNIX_SIGNALS = struct end
