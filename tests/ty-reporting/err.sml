(* NOTE we may want to report big types on many lines? *)
type t = {
  lab_1 : int -> int -> int,
  lab_2 : int * int -> int,
  lab_3 : int * (int -> int),
  lab_4 : (int -> int) -> int,
  lab_5 : int list list,
  lab_6 : int -> int list,
  lab_7 : (int -> int) list,
  lab_8 : int * int list,
  lab_9 : (int * int) list
}
val _ : t = ()
