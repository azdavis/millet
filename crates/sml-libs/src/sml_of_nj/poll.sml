(*!
The Unsafe.Poll structure provides an interface to automatic software polling. The pollEvent flag is
watched (with user-specified frequency), and when it becomes true, a user-specified function is
invoked. This is all done without hardware interrupts; it is useful where the operating system does
not provide an interface to hardware signals, or where the cost of hardware signal-handling is too
high.

Polling does not slow down user code very much, because the polling code is integrated with the
"heap exhausted?" tests that are used to invoke the garbage collector.

In SML/NJ version 110, polling works on the MIPS, Sparc, and Intel x86 architectures.

Soft polling is used to support garbage collection for the MP thread library; fully follow only
steps 1 and 2 for MP. For custom application, follow all steps.

1) build a runtime system for polling by adding "-DSOFT_POLL" to the appropriate
mk.<arch$GT;-<os$GT; make script in src/runtime/objs (see, for example, mk.x86-linux-poll) then, do
a "make clean" followed by a "make -f mk.<arch$GT;-<os$GT;"

2) use the resulting runtime (src/runtime/objs/run.<arch>-<os>) for the following steps

3) start the compiler and set it to emit polling checks:

4) polling is now available in the freshly compiled compiler to use, install a polling handler,
e.g.:

5) set a polling frequency: Unsafe.Poll.setFreq (SOME 50); Frequency (SOME x) sets the polling
interval to approximately x*1000 instructions; frequency NONE disables polling.

6) the installed poll handler will be called after Unsafe.Poll.pollEvent := true; occurs.
!*)
signature POLL = sig
  exception BadPollFreq
  (*!
  pollEvent When this variable becomes true (and the frequency is not NONE), the hander will be
  invoked. pollEvent is accessible to the runtime system and may (of course) be set asynchronously
  by other threads.
  !*)
  val pollEvent : bool ref
  (*!
  setHandler a If a=f , install f as the event handler. When pollEvent becomes true, the current
  continuation k will be saved, pollEvent will be set to false, and f(k) will be called. No polling
  will occur during the execution of f. When f returns some continuation k', then polling will
  resume and k' will be invoked. If a=NONE , disable polling.
  !*)
  val setHandler : (unit Cont.cont -> unit Cont.cont) option -> unit
  (*!
  inqHandler () Returns the current event handler.
  !*)
  val inqHandler : unit -> (unit Cont.cont -> unit Cont.cont) option
  (*!
  setFreq a When a=(SOME i) , sets the polling frequency to i ; that is, approximately every i*1000
  instructions, the pollEvent variable will be checked. When a=NONE , disables polling.
  !*)
  val setFreq : int option -> unit
  (*!
  inqFreq () Returns the current frequency value.
  !*)
  val inqFreq : unit -> int option
end
