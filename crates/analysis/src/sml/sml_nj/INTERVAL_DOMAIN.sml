signature INTERVAL_DOMAIN = sig
  type point
  val compare : (point * point) -> order
  val succ : point -> point
  val pred : point -> point
  val isSucc : (point * point) -> bool
  val minPt : point
  val maxPt : point
end
