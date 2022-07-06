structure HashConsAtom : sig
  type hash_key = Atom.atom
  type obj = hash_key HashCons.obj
  val mk : hash_key -> obj
end = struct end
