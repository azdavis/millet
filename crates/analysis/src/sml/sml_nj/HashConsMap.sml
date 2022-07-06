signature HASH_CONS_MAP = sig
  type 'a obj = 'a HashCons.obj
  type ('a, 'b) map
  val isEmpty : ('a, 'b) map -> bool
  val singleton : ('a obj * 'b) -> ('a, 'b) map
  val insert : ('a, 'b) map * 'a obj * 'b -> ('a, 'b) map
  val insert' : (('a obj * 'b) * ('a, 'b) map) -> ('a, 'b) map
  val insertWith : (('b * 'b) -> 'b) -> ('a, 'b) map * 'a obj * 'b -> ('a, 'b) map
  val insertWithi : (('a obj * 'b * 'b) -> 'b) -> ('a, 'b) map * 'a obj * 'b -> ('a, 'b) map
  val find : ('a, 'b) map * 'a obj -> 'b option
  val lookup : ('a, 'b) map * 'a obj -> 'b
  val inDomain : (('a, 'b) map * 'a obj) -> bool
  val remove : ('a, 'b) map * 'a obj -> ('a, 'b) map * 'b
  val empty : ('a, 'b) map
  val numItems : ('a, 'b) map -> int
  val listItems : ('a, 'b) map -> 'b list
  val listItemsi : ('a, 'b) map -> ('a obj * 'b) list
  val listKeys : ('a, 'b) map -> 'a obj list
  val collate : ('b * 'b -> order) -> (('a, 'b) map * ('a, 'b) map) -> order
  val unionWith : ('b * 'b -> 'b) -> (('a, 'b) map * ('a, 'b) map) -> ('a, 'b) map
  val unionWithi : ('a obj * 'b * 'b -> 'b) -> (('a, 'b) map * ('a, 'b) map) -> ('a, 'b) map
  val intersectWith : ('b * 'c -> 'd) -> (('a, 'b) map * ('a, 'c) map) -> ('a, 'd) map
  val intersectWithi : ('a obj * 'b * 'c -> 'd) -> (('a, 'b) map * ('a, 'c) map) -> ('a, 'd) map
  val mergeWith : ('b option * 'c option -> 'd option) -> (('a, 'b) map * ('a, 'c) map) -> ('a, 'd) map
  val mergeWithi : ('a obj * 'b option * 'c option -> 'd option) -> (('a, 'b) map * ('a, 'c) map) -> ('a, 'd) map
  val app : ('b -> unit) -> ('a, 'b) map -> unit
  val appi : (('a obj * 'b) -> unit) -> ('a, 'b) map -> unit
  val map : ('b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
  val mapi : ('a obj * 'b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
  val fold : ('b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
  val foldi : ('a obj * 'b * 'c -> 'c) -> 'c -> ('a, 'b) map -> 'c
  val filter : ('b -> bool) -> ('a, 'b) map -> ('a, 'b) map
  val filteri : ('a obj * 'b -> bool) -> ('a, 'b) map -> ('a, 'b) map
  val mapPartial : ('b -> 'c option) -> ('a, 'b) map -> ('a, 'c) map
  val mapPartiali : ('a obj * 'b -> 'c option) -> ('a, 'b) map -> ('a, 'c) map
  val exists : ('b -> bool) -> ('a, 'b) map -> bool
  val existsi : ('a obj * 'b -> bool) -> ('a, 'b) map -> bool
  val all : ('b -> bool) -> ('a, 'b) map -> bool
  val alli : ('a obj * 'b -> bool) -> ('a, 'b) map -> bool
end

structure HashConsMap : HASH_CONS_MAP = struct end
