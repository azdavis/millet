functor F (A: sig end) = struct
  datatype t = C
  fun f C = ()
end

structure S = struct end

structure One = F (S)
structure Two = F (S)

val _ = One.f Two.C
