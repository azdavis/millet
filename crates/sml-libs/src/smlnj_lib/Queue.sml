signature QUEUE = sig
  type 'a queue
  exception Dequeue
  val mkQueue : unit -> 'a queue
  val clear : 'a queue -> unit
  val isEmpty : 'a queue -> bool
  val enqueue : 'a queue * 'a -> unit
  val dequeue : 'a queue -> 'a
  val next : 'a queue -> 'a option
  val delete : ('a queue * ('a -> bool)) -> unit
  val head : 'a queue -> 'a
  val peek : 'a queue -> 'a option
  val length : 'a queue -> int
  val contents : 'a queue -> 'a list
  val app : ('a -> unit) -> 'a queue -> unit
  val map : ('a -> 'b) -> 'a queue -> 'b queue
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
end

structure Queue :> QUEUE = struct end
