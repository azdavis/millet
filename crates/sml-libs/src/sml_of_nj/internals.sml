signature INTERNALS = sig
  structure CleanUp : CLEAN_UP
  structure ProfControl : PROF_CONTROL
  structure GC : GC
  (*!
  prHook This is a hook for the top-level print function, which allows it to be rebound.
  !*)
  val prHook : (string -> unit) ref
  (*!
  initSigTbl a Initialize the signal table to the inherited process environment. The argument a is
  ignored.
  !*)
  val initSigTbl : 'a -> unit
  (*!
  clearSigTbl a Clear the signal table of handlers. The argument a is ignored.
  !*)
  val clearSigTbl : 'a -> unit
  (*!
  resetSigTbl a Reset the signal environment to agree with the signal table. The argument a is
  ignored.
  !*)
  val resetSigTbl : 'a -> unit
end

structure Internals : INTERNALS = struct end
