signature HASH_CONS_SET = sig
  type 'a obj = 'a HashCons.obj
  type 'a set
  val empty : 'a set
  val singleton : 'a obj -> 'a set
  val fromList : 'a obj list -> 'a set
  val add : 'a set * 'a obj -> 'a set
  val add' : ('a obj * 'a set) -> 'a set
  val addList : 'a set * 'a obj list -> 'a set
  val subtract : 'a set * 'a obj -> 'a set
  val subtract' : ('a obj * 'a set) -> 'a set
  val subtractList : 'a set * 'a obj list -> 'a set
  val delete : 'a set * 'a obj -> 'a set
  val member : 'a set * 'a obj -> bool
  val isEmpty : 'a set -> bool
  val equal : ('a set * 'a set) -> bool
  val compare : ('a set * 'a set) -> order
  val isSubset : ('a set * 'a set) -> bool
  val disjoint : 'a set * 'a set -> bool
  val numItems : 'a set -> int
  val toList : 'a set -> 'a obj list
  val listItems : 'a set -> 'a obj list
  val union : 'a set * 'a set -> 'a set
  val intersection : 'a set * 'a set -> 'a set
  val difference : 'a set * 'a set -> 'a set
  val map : ('a obj -> 'b obj) -> 'a set -> 'b set
  val mapPartial : ('a obj -> 'b obj option) -> 'a set -> 'b set
  val app : ('a obj -> unit) -> 'a set -> unit
  val foldl : ('a obj * 'b -> 'b) -> 'b -> 'a set -> 'b
  val foldr : ('a obj * 'b -> 'b) -> 'b -> 'a set -> 'b
  val partition : ('a obj -> bool) -> 'a set -> ('a set * 'a set)
  val filter : ('a obj -> bool) -> 'a set -> 'a set
  val all : ('a obj -> bool) -> 'a set -> bool
  val exists : ('a obj -> bool) -> 'a set -> bool
  val find : ('a obj -> bool) -> 'a set -> 'a obj option
end

structure HashConsSet : HASH_CONS_SET = struct end
