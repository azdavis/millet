signature XML_SCHEMA = sig
  type element
  type attribute
  val element : string -> element option
  val preserveWS : element -> bool
  val preserveComment : element -> bool
  val same : element * element -> bool
  val toString : element -> string
  val attribute : (string * string) -> attribute
end
