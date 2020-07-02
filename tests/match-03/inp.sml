val _ =
  case (3, "hey", false, LESS) of
    (3, "guh", _, _) => 0
  | (_, "hey", false, GREATER) => 1
  | (4, _, false, _) => 2
  | (_, _, false, EQUAL) => 3
  | (_, _, false, _) => 4
  | (3, _, true, LESS) => 5
  | (_, "nope", true, LESS) => 6
  | (n, _, true, LESS) => n
  | (_, _, true, EQUAL) => 8
  | (_, _, true, GREATER) => 9
