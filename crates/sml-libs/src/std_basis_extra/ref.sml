signature REF = sig
  datatype ref = datatype ref
  val ! : 'a ref -> 'a
  val := : 'a ref * 'a -> unit
  val exchange : 'a ref * 'a -> 'a
  val swap : 'a ref * 'a ref -> unit
  val app : ('a -> unit) -> 'a ref -> unit
  val map : ('a -> 'b) -> 'a ref -> 'b ref
  val modify : ('a -> 'a) -> 'a ref -> unit
end

structure Ref :> REF = struct end
