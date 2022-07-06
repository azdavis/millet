signature MATCH_TREE = sig
  datatype 'a match_tree = Match of 'a * 'a match_tree list
  val root : 'a match_tree -> 'a
  val nth : ('a match_tree * int) -> 'a (* raises Subscript *)
  val map : ('a -> 'b) -> 'a match_tree -> 'b match_tree
  val app : ('a -> unit) -> 'a match_tree -> unit
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a match_tree -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a match_tree -> 'b
  val find : ('a -> bool) -> 'a match_tree -> 'a option
  val num : 'a match_tree -> int
end

structure MatchTree : MATCH_TREE = struct end
