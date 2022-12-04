//! Some simple tests.

use crate::check::{check, go, Outcome};
use diagnostic_util::Severity;

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
structure val
(**       ^^^ expected a name *)
"#,
  );
}

#[test]
fn missing_rhs() {
  check(
    r#"
    val x
(**     ^ missing right-hand side of declaration *)
"#,
  );
}

#[test]
fn invalid_char() {
  check(
    r#"
val 空条承太郎 = 3
(** ^ invalid source character *)
"#,
  );
}

#[test]
fn unclosed_comment() {
  check(
    r#"
(**       vv unclosed comment *)
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
  );
}

#[test]
fn ty() {
  check(
    r#"
type 'a foo = 'a * {} * int list list * (int -> string) * { a: int, b: string }
  "#,
  );
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
  let ss = &[
    (
      "a.sml",
      r#"
val a = 3
exception Bad
"#,
    ),
    (
      "b.sml",
      r#"
val b = a + 4
fun err s = if s = "bad" then raise Bad else ()
"#,
    ),
    ("sources.mlb", "a.sml b.sml"),
  ];
  go(ss, analysis::StdBasis::Minimal, Outcome::Pass, Severity::Error);
}

#[test]
fn weird_ident() {
  check(
    r#"
type 'a' t = 'a' list
type 'element_type t = 'element_type list
val don'tStarve = ()
val uh''''what0000is______this''''' = ()
"#,
  );
}

#[test]
fn utf8_comment() {
  check(
    r#"
val a = 1
(* 宇宙には、始まりはあるが、終わりはない。無限。 *)
val b = 2
 "#,
  );
}

#[test]
fn utf8_string() {
  check(
    r#"
val _: string = "星にもまた、始まりはあるが、自らの力を持って滅びゆく。有限。"
"#,
  );
}

#[test]
fn withtype() {
  check(
    r#"
datatype foo = Foo of bar
withtype bar = int and quz = foo
val _ = 3 : bar
val _ = Foo 4 : quz
"#,
  );
}

#[test]
fn ty_hole() {
  check(
    r#"
type uh = _ * int
(**       ^ type hole *)
"#,
  );
}
