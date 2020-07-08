(* we might want to raise an error here? *)
exception No = Match
val _ =
  3 handle
    Match => 1
  | No => 2
  | _ => 3
