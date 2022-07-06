signature MONO_HASH_SET = sig
  structure Key : HASH_KEY
  type item = Key.hash_key
  type set
  val mkEmpty : int -> set
  val mkSingleton : item -> set
  val mkFromList : item list -> set
  val toList : set -> item list
  val add : set * item -> unit
  val addc : set -> item -> unit
  val addList : set * item list -> unit
  val subtract : set * item -> unit
  val subtractc : set -> item -> unit
  val subtractList : set * item list -> unit
  val delete : set * item -> bool
  val member : set * item -> bool
  val isEmpty : set -> bool
  val isSubset : (set * set) -> bool
  val numItems : set -> int
  val map : (item -> item) -> set -> set
  val mapPartial : (item -> item option) -> set -> set
  val app : (item -> unit) -> set -> unit
  val fold : (item * 'b -> 'b) -> 'b -> set -> 'b
  val partition : (item -> bool) -> set -> (set * set)
  val filter : (item -> bool) -> set -> unit
  val exists : (item -> bool) -> set -> bool
  val all : (item -> bool) -> set -> bool
  val find : (item -> bool) -> set -> item option
  val listItems : set -> item list
  val without : set * item -> unit
end

functor HashSetFn (Key : HASH_KEY) : MONO_HASH_SET = struct end
