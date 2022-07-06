signature MONO_HASH2_TABLE = sig
  structure Key1 : HASH_KEY
  structure Key2 : HASH_KEY
  type 'a hash_table
  val mkTable : (int * exn) -> 'a hash_table
  val clear : 'a hash_table -> unit
  val insert : 'a hash_table -> (Key1.hash_key * Key2.hash_key * 'a) -> unit
  val inDomain1 : 'a hash_table -> Key1.hash_key -> bool
  val inDomain2 : 'a hash_table -> Key2.hash_key -> bool
  val lookup1 : 'a hash_table -> Key1.hash_key -> 'a
  val lookup2 : 'a hash_table -> Key2.hash_key -> 'a
  val find1 : 'a hash_table -> Key1.hash_key -> 'a option
  val find2 : 'a hash_table -> Key2.hash_key -> 'a option
  val remove1 : 'a hash_table -> Key1.hash_key -> 'a
  val remove2 : 'a hash_table -> Key2.hash_key -> 'a
  val numItems : 'a hash_table -> int
  val listItems : 'a hash_table -> 'a list
  val listItemsi : 'a hash_table -> (Key1.hash_key * Key2.hash_key * 'a) list
  val app : ('a -> unit) -> 'a hash_table -> unit
  val appi : ((Key1.hash_key * Key2.hash_key * 'a) -> unit) -> 'a hash_table -> unit
  val map : ('a -> 'b) -> 'a hash_table -> 'b hash_table
  val mapi : ((Key1.hash_key * Key2.hash_key * 'a) -> 'b) -> 'a hash_table -> 'b hash_table
  val fold : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
  val foldi : ((Key1.hash_key * Key2.hash_key * 'a * 'b) -> 'b) -> 'b
  val filter : ('a -> bool) -> 'a hash_table -> unit
  val filteri : ((Key1.hash_key * Key2.hash_key * 'a) -> bool) -> 'a hash_table -> unit
  val copy : 'a hash_table -> 'a hash_table
  val bucketSizes : 'a hash_table -> (int list * int list)
end

functor Hash2TableFn (
  structure Key1 : HASH_KEY
  structure Key2 : HASH_KEY
) : MONO_HASH2_TABLE = struct end
