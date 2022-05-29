use crate::check::check;

mod dec;
mod exp;

#[test]
#[ignore = "todo for new"]
fn pat() {
  check(
    r#"
datatype d = D of int * int
infix D
val _ = fn
  tup as (1, x: d, false, "hi", _, 0w123, 3 D 4, 0xbeef, 0wx123beef, [a, 3, c], #"b")  => 1
| _ => 2
  "#,
  )
}

#[test]
fn ty() {
  check(
    r#"
(* old-todo *)
type 'a foo = 'a * {} * int list list * (int -> string) * { a: int, b: string }
  "#,
  )
}
