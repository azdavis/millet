signature ORD_KEY = sig
  type ord_key
  val compare : ord_key * ord_key -> order
end
