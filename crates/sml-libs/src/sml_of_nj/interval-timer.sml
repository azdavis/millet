signature INTERVAL_TIMER = sig
  (*!
  tick () Returns the granularity of the interval timer.
  !*)
  val tick : unit -> Time.time
  (*!
  See setIntTimer.
  !*)
  val setIntTimer : Time.time option -> unit
end

structure IntervalTimer : INTERVAL_TIMER = struct end
