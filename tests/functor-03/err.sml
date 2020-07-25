functor F (A: sig
  exception Foo
end) = struct end
structure S = F (struct
  val Foo = Match
end)
