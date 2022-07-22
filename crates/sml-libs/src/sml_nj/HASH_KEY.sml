signature HASH_KEY = sig
  type hash_key
  val hashVal : hash_key -> word
  val sameKey : (hash_key * hash_key) -> bool
end
