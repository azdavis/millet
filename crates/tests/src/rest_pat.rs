//! `{a, ...}` patterns, as used in `#a`.

use crate::check::{check, fail};

#[test]
fn multi_rest() {
  check(
    r#"
val _ = fn {a, ..., ...} => 3
(**        ^^^^^^^^^^^^^ multiple `...` *)
"#,
  );
}

#[test]
fn rest_not_last() {
  check(
    r#"
val _ = fn {..., a} => 3
(**        ^^^^^^^^ `...` must come last *)
"#,
  );
}

#[test]
fn selector_smoke() {
  check(
    r#"
val _ : int = #1 (3, "hi")
"#,
  );
}

#[test]
fn as_rest() {
  check(
    r#"
fun combine {a, b} = a + b
val f = fn x as {b, ...} => combine x + 4 + b
"#,
  );
}

#[test]
fn unresolved_smoke() {
  check(
    r#"
val f = #foo
(**     ^^^^ cannot resolve `...` in type: { foo : ?a, ... } *)
"#,
  );
}

#[test]
fn unresolved_complex() {
  check(
    r#"
fun f x = #foo x + #bar x + 1.1
(**                ^^^^ cannot resolve `...` in type: { bar : <num>, foo : <num>, ... } *)
"#,
  );
}

#[test]
fn resolved_complex_1() {
  check(
    r#"
type t = { foo: real, bar: real }
fun f (x: t) = #foo x + #bar x + 1.1
"#,
  );
}

#[test]
fn resolved_complex_2() {
  check(
    r#"
type t = { foo: real, bar: real }
fun f x = #foo x + #bar x + 1.1 + (x:t; 0.0)
"#,
  );
}

#[test]
fn update_complex() {
  check(
    r#"
fun f x =
  let
    val _ = #foo x + 1
  in
    if #foo x then 1 else 2
(**    ^^^^^^ expected bool, found int *)
  end
"#,
  );
}

#[test]
fn infer_across_decs() {
  fail(
    r#"
type bar = {x: int, y: string}
fun f b = #x bar
fun g (b : bar) = f b
"#,
  );
}
