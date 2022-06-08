use crate::check::check;

#[test]
fn across_var() {
  check(
    r#"
fun 'a f (x: 'a) = let val y = x in y false; y end
(**                                 ^ expected a function type, got 'a *)
"#,
  );
}

#[test]
fn bound_at_fun_1() {
  check(
    r#"
fun bar (x: int): unit = ()
fun 'a f (id: 'a -> 'a) x = bar (id x)
(**                              ^^^^ expected int, found 'a *)
"#,
  );
}

#[test]
fn bound_at_fun_2() {
  check(
    r#"
fun 'a f (id: 'a -> 'a) x = id x + 1
(**                         ^^^^^^^^ expected _ * _ with word, real, or int, found 'a * int *)
"#,
  );
}

#[test]
fn annotate() {
  check(
    r#"
val 'a _ = false: 'a
(**        ^^^^^^^^^ expected 'a, found bool *)
"#,
  );
}

#[test]
fn type_datatype() {
  check(
    r#"
type 'a heh = 'a list
datatype 'a bad = Bad of 'a
val _: int heh = [1]
val _ = Bad: unit
(**     ^^^^^^^^^ expected unit, found _ -> _ bad *)
"#,
  );
}

#[test]
fn apply() {
  check(
    r#"
fun ('t, 'u) apply (f: 't -> 'u) (x: 't): 'u = f x
val _ = apply op+ (1, false)
(**               ^^^^^^^^^^ expected _ * _, found int * bool *)
"#,
  );
}

#[test]
fn different_vars() {
  check(
    r#"
fun ('a, 'b) f (xs: 'a list) (x: 'b) =
    x :: xs
(** ^^^^^^^ expected _ * _ list, found _ * _ *)
"#,
  );
}
