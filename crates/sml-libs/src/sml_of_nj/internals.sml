signature INTERNALS = sig
  structure CleanUp : CLEAN_UP
  structure ProfControl : PROF_CONTROL
  structure GC : GC
  val prHook : (string -> unit) ref
  val initSigTbl : 'a -> unit
  val clearSigTbl : 'a -> unit
  val resetSigTbl : 'a -> unit
end

structure Internals : INTERNALS = struct end
