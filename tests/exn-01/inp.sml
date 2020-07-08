exception Foo
and Bar of int
and Guh = Match
val _ =
  3 handle
    Foo => 1
  | Bar x => x
  | Match => 2
  | _ => 3
fun bar x = raise Bar x
val _ = 1 + bar 2 + (raise Foo) + 3
val _ = bar 3 andalso raise Guh
