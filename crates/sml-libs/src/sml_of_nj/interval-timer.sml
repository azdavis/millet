signature INTERVAL_TIMER = sig
  val tick : unit -> Time.time
  val setIntTimer : Time.time option -> unit
end

structure IntervalTimer : INTERVAL_TIMER = struct end
