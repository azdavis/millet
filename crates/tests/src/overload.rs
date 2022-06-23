use crate::check::check;

#[test]
fn curry_add() {
  check(
    r#"
fun add a b = a + b
val _ = add false
(**         ^^^^^ expected int, found bool *)
"#,
  );
}

#[test]
fn op_add_ok() {
  check(
    r#"
val add = op+
val _ = add (1, 2)
"#,
  );
}

#[test]
fn op_add_err() {
  check(
    r#"
val add = op+
val _ = add (false, true)
(**         ^^^^^^^^^^^^^ expected int * int, found bool * bool *)
"#,
  );
}

#[test]
fn immediately_solve_to_default() {
  check(
    r#"
val add = op+
val _ = add (1.1, 2.2)
(**         ^^^^^^^^^^ expected int * int, found real * real *)
val _ = add (1, 2)
"#,
  );
}

#[test]
fn explicit_annotate() {
  check(
    r#"
val add = op+ : real * real -> real
val _ = add (1.1, 2.2)
val _ = add (1, 2)
(**         ^^^^^^ expected real * real, found int * int *)
"#,
  );
}

#[test]
fn top_level_seq() {
  check(
    r#"
val add = op+
(* make this a top-dec level seq *)
signature S = sig end
val _ = add (1.1, 2.2)
(**         ^^^^^^^^^^ expected int * int, found real * real *)
"#,
  );
}

#[test]
fn std_lib_ops() {
  check(
    r#"
(* abs *)
val _: int = abs 1
val _: real = abs 1.1
(* tilde. put a space so the ~ is a function, not part of the literal. *)
val _: int = ~ 1
val _: real = ~ 1.1
(* div *)
val _: int = 1 div 1
val _: word = 0w0 div 0w0
(* mod *)
val _: int = 1 mod 1
val _: word = 0w0 mod 0w0
(* star *)
val _: int = 1 * 1
val _: word = 0w0 * 0w0
val _: real = 1.1 * 1.1
(* slash *)
val _: real = 1.1 / 1.1
(* plus *)
val _: int = 1 + 1
val _: word = 0w0 + 0w0
val _: real = 1.1 + 1.1
(* minus *)
val _: int = 1 - 1
val _: word = 0w0 - 0w0
val _: real = 1.1 - 1.1
(* lt *)
val _: bool = 1 < 1
val _: bool = 0w0 < 0w0
val _: bool = 1.1 < 1.1
val _: bool = "e" < "e"
val _: bool = #"e" < #"e"
(* lt eq *)
val _: bool = 1 <= 1
val _: bool = 0w0 <= 0w0
val _: bool = 1.1 <= 1.1
val _: bool = "e" <= "e"
val _: bool = #"e" <= #"e"
(* gt *)
val _: bool = 1 > 1
val _: bool = 0w0 > 0w0
val _: bool = 1.1 > 1.1
val _: bool = "e" > "e"
val _: bool = #"e" > #"e"
(* gt eq *)
val _: bool = 1 >= 1
val _: bool = 0w0 >= 0w0
val _: bool = 1.1 >= 1.1
val _: bool = "e" >= "e"
val _: bool = #"e" >= #"e"
"#,
  );
}

#[test]
fn must_solve_to_single_overloaded_type() {
  check(
    r#"
val _ = 1.1 + 1
(**     ^^^^^^^ expected _ * _, found real * int *)
"#,
  );
}

#[test]
fn overload_err() {
  check(
    r#"
val  _ = false + true
(**      ^^^^^^^^^^^^ expected _ * _ with word, real, or int, found bool * bool *)
"#,
  );
}

#[test]
fn three_int() {
  check(
    r#"
fun f a b c = a + b + c
val _ = f : unit
(**     ^^^^^^^^ expected unit, found int -> int -> int -> int *)
"#,
  );
}

#[test]
fn three_real() {
  check(
    r#"
fun f a b c = a + b + c + 1.1
val _ = f : unit
(**     ^^^^^^^^ expected unit, found real -> real -> real -> real *)
"#,
  );
}

#[test]
fn add_div() {
  check(
    r#"
fun mid a b = (a + b) div 2
"#,
  );
}

#[test]
fn abs_sub_add() {
  check(
    r#"
fun hm a b = (abs (a - b), a + b)
"#,
  );
}
