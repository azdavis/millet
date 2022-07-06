signature PRIORITY = sig
  type priority
  val compare : (priority * priority) -> order
  type item
  val priority : item -> priority
end

signature MONO_PRIORITYQ = sig
  type item
  type queue
  val empty : queue
  val singleton : item -> queue
  val fromList : item list -> queue
  val insert : (item * queue) -> queue
  val remove : queue -> (item * queue)
  val next : queue -> (item * queue) option
  val findAndRemove : queue * (item -> bool) -> (item * queue) option
  val delete : queue * (item -> bool) -> queue
  val merge : (queue * queue) -> queue
  val numItems : queue -> int
  val isEmpty : queue -> bool
end

functor LeftPriorityQFn (P : PRIORITY) : MONO_PRIORITYQ = struct end
