use crate::check::{check, check_multi};

mod dec;
mod exp;

#[test]
fn empty() {
  check("");
}

#[test]
fn undefined() {
  check(
    r#"
val _ = nope
(**     ^^^^ undefined value: nope *)
"#,
  );
}

#[test]
fn no_top_items() {
  check(
    r#"
    and
(** ^^^ expected a top-level item *)
"#,
  );
}

#[test]
fn expected() {
  check(
    r#"
val _ 3
(**   ^ expected `=` *)
"#,
  );
}

#[test]
fn unknown_byte() {
  check(
    r#"
val 空条承太郎 = 1
(** ^^^ invalid source character *)
"#,
  );
}

#[test]
fn unmatched_close_comment() {
  check(
    r#"
val x = 3 *)
(**       ^^ unmatched close comment *)
"#,
  );
}

#[test]
fn unmatched_open_comment() {
  check(
    r#"
(**       vv unmatched open comment *)
val x = 3 (*
"#,
  );
}

#[test]
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
type 'a foo = 'a * {} * int list list * (int -> string) * { a: int, b: string }
  "#,
  )
}

#[test]
fn tuple() {
  check(
    r#"
val _: int * bool = (3, false)
"#,
  );
}

#[test]
fn structure() {
  check(
    r#"
structure S = struct
  val x = 3
end
val _: int = S.x
"#,
  );
}

#[test]
fn multi() {
  check_multi(&[
    r#"
val a = 3
"#,
    r#"
val b = a
"#,
  ]);
}
