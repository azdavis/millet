structure SExp : sig
  datatype value =
    SYMBOL of Atom.atom
  | BOOL of bool
  | INT of IntInf.int
  | FLOAT of real
  | STRING of string
  | QUOTE of value
  | LIST of value list
  val same : value * value -> bool
  val compare : value * value -> order
end = struct end
