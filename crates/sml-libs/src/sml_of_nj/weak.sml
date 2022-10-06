signature WEAK = sig
  type 'a weak
  val weak : 'a -> 'a weak
  val strong : 'a weak -> 'a option
  type weak'
  val weak' : 'a -> weak'
  val strong' : weak' -> bool
end

structure Weak : WEAK = struct end
