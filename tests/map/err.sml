datatype 'a option = None | Some of 'a

fun option_map f opt =
  case opt of
    None => None
  | Some x => Some (f x)

fun list_map f xs =
  case xs of
    [] => []
  | x :: xs => f x :: list_map f xs

val _: unit = (option_map, list_map)
