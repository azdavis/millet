signature MONO_HASH_TABLE = sig
  structure Key : HASH_KEY
  type 'a hash_table
  val mkTable : (int * exn) -> 'a hash_table
  val clear : 'a hash_table -> unit
  val insert : 'a hash_table -> (Key.hash_key * 'a) -> unit
  val inDomain : 'a hash_table -> Key.hash_key -> bool
  val lookup : 'a hash_table -> Key.hash_key -> 'a
  val find : 'a hash_table -> Key.hash_key -> 'a option
  val remove : 'a hash_table -> Key.hash_key -> 'a
  val numItems : 'a hash_table -> int
  val listItems : 'a hash_table -> 'a list
  val listItemsi : 'a hash_table -> (Key.hash_key * 'a) list
  val app : ('a -> unit) -> 'a hash_table -> unit
  val appi : ((Key.hash_key * 'a) -> unit) -> 'a hash_table -> unit
  val map : ('a -> 'b) -> 'a hash_table -> 'b hash_table
  val mapi : ((Key.hash_key * 'a) -> 'b) -> 'a hash_table -> 'b hash_table
  val fold : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
  val foldi : ((Key.hash_key * 'a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
  val modify : ('a -> 'a) -> 'a hash_table -> unit
  val modifyi : ((Key.hash_key * 'a) -> 'a) -> 'a hash_table -> unit
  val filter : ('a -> bool) -> 'a hash_table -> unit
  val filteri : ((Key.hash_key * 'a) -> bool) -> 'a hash_table -> unit
  val copy : 'a hash_table -> 'a hash_table
  val bucketSizes : 'a hash_table -> int list
end

structure AtomTable :> MONO_HASH_TABLE
  where type Key.hash_key = Atom.atom
  = struct end

structure IntHashTable :> MONO_HASH_TABLE
  where type Key.hash_key = int
  = struct end

structure WordHashTable :> MONO_HASH_TABLE
  where type Key.hash_key = word
  = struct end
