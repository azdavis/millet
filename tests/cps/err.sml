datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
fun find t p ok err =
  case t of
    Empty => err ()
  | Node (left, x, right) =>
      if p x then
        ok x
      else
        find left p ok (fn () => find right p ok err)
val _ : unit = find
