functor HashConsGroundFn (T : HASH_KEY) : sig
  type hash_key = T.hash_key
  type obj = hash_key HashCons.obj
  val mk : hash_key -> obj
end = struct end
