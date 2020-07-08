val _ =
  case (1, 3.3, "hey", LESS, false) of
    (1, _, _, LESS, _) => 0
  | (3, x, "foo", GREATER, true) => 1
  | (_, _, "nope", EQUAL, _) => 2
  | (_, _, "guy", EQUAL, _) => 3
  | (_, _, "thing", EQUAL, _) => 4
  | (_, _, _, EQUAL, true) => 5
  | (x, _, _, EQUAL, false) => x
  | (_, _, _, LESS, true) => 5
  | (x, _, _, LESS, false) => x
  | (_, _, _, GREATER, true) => 5
  | (x, _, _, GREATER, false) => x
