functor F (A: sig end) = struct end
(* ok to pass extra things in the struct *)
structure S = F (struct
  type t = int
end)
