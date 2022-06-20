(* https://github.com/SMLFamily/BasisLibrary/tree/8d5681c77a6ef8fa4690a15f4d5cba8b3657a7ea/Code/2015/005 *)
signature FN = sig
  val id : 'a -> 'a
  val const : 'a -> 'b -> 'a
  val apply : ('a -> 'b) * 'a -> 'b
  val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
  val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
  val flip : ('a * 'b -> 'c) -> ('b * 'a -> 'c)
  val repeat : int -> ('a -> 'a) -> ('a -> 'a)
  val equal : ''a -> ''a -> bool
  val notEqual : ''a -> ''a -> bool
end

structure Fn :> FN = struct end
