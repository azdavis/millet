functor KeywordFn () : sig
  type token
  type pos
  val keyword : (string * pos * pos) -> token
end = struct end
