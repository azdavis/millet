signature LIST_XPROD = sig
  val app : (('a * 'b) -> unit) -> ('a list * 'b list) -> unit
  val map : (('a * 'b) -> 'c) -> ('a list * 'b list) -> 'c list
  val fold : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
  val appX : (('a * 'b) -> unit) -> ('a list * 'b list) -> unit
  val mapX : (('a * 'b) -> 'c) -> ('a list * 'b list) -> 'c list
  val foldX : (('a * 'b * 'c) -> 'c) -> ('a list * 'b list) -> 'c -> 'c
end

structure ListXProd : LIST_XPROD = struct end
