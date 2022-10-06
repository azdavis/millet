signature UREF = sig
  type 'a uref
  val uRef: 'a -> 'a uref
  val equal: 'a uref * 'a uref -> bool
  val !! : 'a uref -> 'a
  val update : 'a uref * 'a -> unit
  val unify : ('a * 'a -> 'a) -> 'a uref * 'a uref -> bool
  val union : 'a uref * 'a uref -> bool
  val link : 'a uref * 'a uref -> bool
end

structure URef : UREF = struct end
