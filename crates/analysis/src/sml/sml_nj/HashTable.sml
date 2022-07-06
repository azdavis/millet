structure HashTable : sig
  type ('a, 'b) hash_table
  val mkTable : (('a -> word) * (('a * 'a) -> bool)) -> (int * exn) -> ('a,'b) hash_table
  val clear : ('a, 'b) hash_table -> unit
  val insert : ('a, 'b) hash_table -> ('a * 'b) -> unit
  val inDomain : ('a, 'b) hash_table -> 'a -> bool
  val lookup : ('a, 'b) hash_table -> 'a -> 'b
  val find : ('a, 'b) hash_table -> 'a -> 'b option
  val remove : ('a, 'b) hash_table -> 'a -> 'b
  val numItems : ('a, 'b) hash_table -> int
  val listItems : ('a, 'b) hash_table -> 'b list
  val listItemsi : ('a, 'b) hash_table -> ('a * 'b) list
  val app : ('b -> unit) -> ('a, 'b) hash_table -> unit
  val appi : (('a * 'b) -> unit) -> ('a, 'b) hash_table -> unit
  val map : ('b -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
  val mapi : (('a * 'b) -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
  val fold : (('b *'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
  val foldi : (('a * 'b * 'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
  val modify : ('b -> 'b) -> ('a, 'b) hash_table -> unit
  val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) hash_table -> unit
  val filter : ('b -> bool) -> ('a, 'b) hash_table -> unit
  val filteri : (('a * 'b) -> bool) -> ('a, 'b) hash_table -> unit
  val copy : ('a, 'b) hash_table -> ('a, 'b) hash_table
  val bucketSizes : ('a, 'b) hash_table -> int list
end = struct end
