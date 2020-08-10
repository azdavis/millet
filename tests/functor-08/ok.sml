signature SIG = sig
  type t
end

functor Id (S: SIG) = S

structure A = struct
  type t = int
end

structure B = Id (A)

val _: A.t = 3
val _: B.t = 3
