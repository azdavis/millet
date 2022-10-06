structure JSON : sig
  datatype value =
    OBJECT of (string * value) list
  | ARRAY of value list
  | NULL
  | BOOL of bool
  | INT of IntInf.int
  | FLOAT of real
  | STRING of string
end = struct end
