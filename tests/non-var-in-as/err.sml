exception Bad
val _ =
  case 3 of
    Bad as _ => 1
  | _ => 2
