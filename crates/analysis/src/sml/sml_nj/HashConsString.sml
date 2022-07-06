structure HashConsString : sig
  type hash_key = string
  type obj = hash_key HashCons.obj
  val mk : hash_key -> obj
end = struct end
