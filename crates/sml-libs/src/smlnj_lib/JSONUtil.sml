structure JSONUtil : sig
  exception NotBool of JSON.value
  exception NotInt of JSON.value
  exception NotNumber of JSON.value
  exception NotString of JSON.value
  exception NotObject of JSON.value
  exception FieldNotFound of JSON.value * string
  exception NotArray of JSON.value
  exception ArrayBounds of JSON.value * int
  exception ElemNotFound of JSON.value
  val exnMessage : exn -> string
  val asBool : JSON.value -> bool
  val asInt : JSON.value -> Int.int
  val asIntInf : JSON.value -> IntInf.int
  val asNumber : JSON.value -> Real.real
  val asString : JSON.value -> string
  val findField : JSON.value -> string -> JSON.value option
  val lookupField : JSON.value -> string -> JSON.value
  val hasField : string -> JSON.value -> bool
  val testField : string -> (JSON.value -> bool) -> JSON.value -> bool
  val asArray : JSON.value -> JSON.value vector
  val arrayMap : (JSON.value -> 'a) -> JSON.value -> 'a list
  datatype edge = SEL of string | SUB of int | FIND of JSON.value -> bool
  type path = edge list
  val get : JSON.value * path -> JSON.value
  val replace : JSON.value * path * JSON.value -> JSON.value
  val insert : JSON.value * path * string * JSON.value -> JSON.value
  val append : JSON.value * path * JSON.value list -> JSON.value
end = struct end
