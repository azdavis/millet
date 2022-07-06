(*!
The structure Posix.Signal defines the symbolic names of all the POSIX signals (see Section 3.3 of
the POSIX standard 1003.1,1996[CITE]), and provides conversion functions between them and their
underlying representations.
!*)
signature POSIX_SIGNAL = sig
  (*!
  A POSIX signal, an asynchronous notification of an event.
  !*)
  eqtype signal
  (*!
  These convert between a signal identifier and its underlying integer representation. Note that
  fromWord does not check that the result corresponds to a valid POSIX signal.
  !*)
  val toWord : signal -> SysWord.word
  val fromWord : SysWord.word -> signal
  val abrt : signal
  val alrm : signal
  val bus : signal
  val fpe : signal
  val hup : signal
  val ill : signal
  val int : signal
  val kill : signal
  val pipe : signal
  val quit : signal
  val segv : signal
  val term : signal
  val usr1 : signal
  val usr2 : signal
  val chld : signal
  val cont : signal
  val stop : signal
  val tstp : signal
  val ttin : signal
  val ttou : signal
end

structure Signal : POSIX_SIGNAL = struct end
