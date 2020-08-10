structure A = struct
  datatype t = B | C
end

signature SIG = sig
  datatype t = B | C
end

functor F (Arg: SIG) = struct
  open Arg
end

structure R = F (A)

val _ =
  case A.B of
    R.B => 1
  | R.C => 2
