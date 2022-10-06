structure PropList : sig
  type holder
  val newHolder : unit -> holder
  val hasProps : holder -> bool
  val clearHolder : holder -> unit
  val sameHolder : (holder * holder) -> bool
  val newProp : (('a -> holder) * ('a -> 'b)) -> { peekFn : 'a -> 'b option, getFn : 'a -> 'b, setFn : ('a * 'b) -> unit, clrFn : 'a -> unit }
  val newFlag : ('a -> holder) -> { getFn : 'a -> bool, setFn : ('a * bool) -> unit }
end = struct end
