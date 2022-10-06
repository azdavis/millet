signature INTERVAL_SET = sig
  structure D : INTERVAL_DOMAIN
  type item = D.point
  type interval = (item * item)
  type set
  val empty : set
  val universe : set
  val singleton : item -> set
  val interval : item * item -> set
  val isEmpty : set -> bool
  val isUniverse : set -> bool
  val member : set * item -> bool
  val items : set -> item list
  val intervals : set -> interval list
  val add : set * item -> set
  val add' : item * set -> set
  val addInt : set * interval -> set
  val addInt' : interval * set -> set
  val complement : set -> set
  val union : (set * set) -> set
  val intersect : (set * set) -> set
  val difference : (set * set) -> set
  val app : (item -> unit) -> set -> unit
  val foldl : (item * 'a -> 'a) -> 'a -> set -> 'a
  val foldr : (item * 'a -> 'a) -> 'a -> set -> 'a
  val filter : (item -> bool) -> set -> set
  val exists : (item -> bool) -> set -> bool
  val all : (item -> bool) -> set -> bool
  val appInt : (interval -> unit) -> set -> unit
  val foldlInt : (interval * 'a -> 'a) -> 'a -> set -> 'a
  val foldrInt : (interval * 'a -> 'a) -> 'a -> set -> 'a
  val filterInt : (interval -> bool) -> set -> set
  val existsInt : (interval -> bool) -> set -> bool
  val allInt : (interval -> bool) -> set -> bool
  val compare : set * set -> order
  val isSubset : set * set -> bool
end

functor IntervalSetFn (D : INTERVAL_DOMAIN) : INTERVAL_SET = struct end
