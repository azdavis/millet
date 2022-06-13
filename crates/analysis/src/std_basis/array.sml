signature ARRAY = sig
  eqtype 'a array
  type 'a vector = 'a Vector.vector
  val maxLen : int
  val array : int * 'a -> 'a array
  val fromList : 'a list -> 'a array
  val tabulate : int * (int -> 'a) -> 'a array
  val length : 'a array -> int
  val sub : 'a array * int -> 'a
  val update : 'a array * int * 'a -> unit
  val vector : 'a array -> 'a vector
  val copy : {src : 'a array, dst : 'a array, di : int} -> unit
  val copyVec : {src : 'a vector, dst : 'a array, di : int} -> unit
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
end

structure Array :> ARRAY = struct end
