(*!
This module creates and manipulates suspensions for lazy evaluation.
!*)
signature SUSP = sig
  type 'a susp
  (*!
  delay f Make a suspension from function f.
  !*)
  val delay : (unit -> 'a) -> 'a susp
  (*!
  force s If s has never been forced before, evaluate its underlying f function, save the result,
  and return the result. If s has been previously forced, return the result from last time.
  !*)
  val force : 'a susp -> 'a
end

structure Susp : SUSP = struct end
