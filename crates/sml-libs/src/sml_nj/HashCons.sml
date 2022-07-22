signature HASH_CONS = sig
  type 'a tbl
  val new : {eq : 'a * 'a -> bool} -> 'a tbl
  val clear : 'a tbl -> unit
  type 'a obj = { nd : 'a, tag : word, hash : word }
  val node : 'a obj -> 'a
  val tag : 'a obj -> word
  val same : ('a obj * 'a obj) -> bool
  val compare : ('a obj * 'a obj) -> order
  val cons0 : 'a tbl -> (word * 'a) -> 'a obj
  val cons1 : 'a tbl -> (word * ('b obj -> 'a)) -> 'b obj -> 'a obj
  val cons2 : 'a tbl -> (word * ('b obj * 'c obj -> 'a)) -> 'b obj * 'c obj -> 'a obj
  val cons3 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj -> 'a)) -> 'b obj * 'c obj * 'd obj -> 'a obj
  val cons4 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj * 'e obj -> 'a)) -> 'b obj * 'c obj * 'd obj * 'e obj -> 'a obj
  val cons5 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj * 'e obj * 'f obj -> 'a)) -> 'b obj * 'c obj * 'd obj * 'e obj * 'f obj -> 'a obj
  val consList : 'a tbl -> (word * ('b obj list -> 'a)) -> 'b obj list -> 'a obj
  val consR1 : 'a tbl -> (word * ('b obj -> 'a) * ('r -> 'b obj)) -> 'r -> 'a obj
  val consR2 : 'a tbl -> (word * ('b obj * 'c obj -> 'a) * ('r -> 'b obj * 'c obj)) -> 'r -> 'a obj
  val consR3 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj -> 'a) * ('r -> 'b obj * 'c obj * 'd obj)) -> 'r -> 'a obj
  val consR4 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj * 'e obj -> 'a) * ('r -> 'b obj * 'c obj * 'd obj * 'e obj)) -> 'r -> 'a obj
  val consR5 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj * 'e obj * 'f obj -> 'a) * ('r -> 'b obj * 'c obj * 'd obj * 'e obj * 'f obj)) -> 'r -> 'a obj
end

structure HashCons : HASH_CONS = struct end
