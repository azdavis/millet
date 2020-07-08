type e = int list option * int ref -> int * bool -> int list
val n = 1 + 2 + 3 + 4 * 5 * 6 * 7 + 8 + 9
val ys = 1 + 2 :: 3 * 4 :: 5 div 6 :: nil
val _ =
  case ys of
    [] => n
  | [x] => x
  | x :: y :: _ => x + y * n
