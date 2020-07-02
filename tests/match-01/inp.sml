datatype hmm = A | B of int | C of hmm | D of string
val _ =
  case A of
    A => 0
  | B 1 => 1
  | B 3 => 2
  | B 123 => 3
  | C A => 4
  | D "foo" => 5
  | D "bar" => 6
  | C (C (C A)) => 7
  | D "quz" => 8
  | C (D "guh") => 9
  | D _ => 10
  | C (B 3) => 11
  | C (B n) => n
  | C (D "hey") => 13
  | B 123 => 14
