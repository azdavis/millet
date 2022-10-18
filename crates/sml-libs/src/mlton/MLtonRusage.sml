signature MLTON_RUSAGE = sig
  type t = {utime : Time.time, (* user time *) stime : Time.time} (* system time *)
  val measureGC : bool -> unit
  val rusage : unit -> {children : t, gc : t, self : t}
end
