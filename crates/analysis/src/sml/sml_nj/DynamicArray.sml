structure DynamicArray : sig
  type 'a array
  val array : (int * 'a) -> 'a array
  val subArray : ('a array * int * int) -> 'a array
  val fromList : 'a list * 'a -> 'a array
  val fromVector : 'a vector * 'a -> 'a array
  val toList : 'a array -> 'a list
  val toVector : 'a array -> 'a vector
  val tabulate: (int * (int -> 'a) * 'a) -> 'a array
  val default : 'a array -> 'a
  val sub : ('a array * int) -> 'a
  val update : ('a array * int * 'a) -> unit
  val bound : 'a array -> int
  val truncate : ('a array * int) -> unit
  val appi : (int * 'a -> unit) -> 'a array -> unit
  val app : ('a -> unit) -> 'a array -> unit
  val modifyi : (int * 'a -> 'a) -> 'a array -> unit
  val modify : ('a -> 'a) -> 'a array -> unit
  val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val findi : (int * 'a -> bool) -> 'a array -> (int * 'a) option
  val find : ('a -> bool) -> 'a array -> 'a option
  val exists : ('a -> bool) -> 'a array -> bool
  val all : ('a -> bool) -> 'a array -> bool
  val collate : ('a * 'a -> order) -> 'a array * 'a array -> order
  val vector : 'a array -> 'a vector
end = struct end
