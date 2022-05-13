use crate::check::check;

#[test]
fn t_01() {
  check(
    r#"
fun 'a f (x: 'a) = let val y = x in y false; y end
(**                                 ^^^^^^^ not a function type: '22 *)
"#,
  );
}

#[test]
fn t_02() {
  check(
    r#"
fun bar (x: int): unit = ()
fun 'a f (id: 'a -> 'a) x = bar (id x)
(**                         ^^^^^^^^^^ mismatched types: expected int, found '25 *)
"#,
  );
}

#[test]
fn t_03() {
  check(
    r#"
fun 'a f (id: 'a -> 'a) x = id x + 1
(**                         ^^^^^^^^ mismatched types: expected '22, found int *)
"#,
  );
}

#[test]
fn t_04() {
  check(
    r#"
    val 'a _: 'a = false
(** ^^^^^^^^^^^^^^^^^^^^ mismatched types: expected '22, found bool *)
"#,
  );
}

#[test]
fn t_05() {
  check(
    r#"
type 'a heh = 'a list
datatype 'a bad = Bad of 'a
val _: int heh = [1]
    val _: unit = Bad
(** ^^^^^^^^^^^^^^^^^ mismatched types: expected unit, found '27 -> '27 bad *)
"#,
  );
}

#[test]
fn t_06() {
  check(
    r#"
fun ('t, 'u) apply (f: 't -> 'u) (x: 't): 'u = f x
val _ = apply op+ (1, false)
(**     ^^^^^^^^^^^^^^^^^^^^ mismatched types: expected int, found bool *)
"#,
  );
}

#[test]
fn t_07() {
  check(
    r#"
fun ('a, 'b) f (xs: 'a list) (x: 'b) =
    x :: xs
(** ^^^^^^^ mismatched types: expected '23, found '22 *)
"#,
  );
}
