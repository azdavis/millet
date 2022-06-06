use crate::check::{check, fail};

#[test]
fn t_00() {
  fail(
    r#"
fun f a b = a + b
val _ = f: unit
(**     ^^^^^^^ expected unit, found int -> int -> int *)
"#,
  );
}

#[test]
fn t_01() {
  check(
    r#"
val add = op+
val _ = add (1, 2)
"#,
  );
}

#[test]
fn t_02() {
  fail(
    r#"
val add = op+
val _ = add (1.1, 2.2)
val _ = add (1, 2)
(**     ^^^^^^^^^^ expected real, found int *)
"#,
  );
}

#[test]
fn t_03() {
  fail(
    r#"
val add = op+
(* make this a top-dec level seq *)
signature S = sig end
val _ = add (1.1, 2.2)
(**     ^^^^^^^^^^^^^^ expected int, found real *)
"#,
  );
}

#[test]
fn t_04() {
  check(
    r#"
(* abs *)
val _: int = abs 1
val _: real = abs 1.1
(* tilde. put a space so the ~ is a function, not part of the constant. *)
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
fn t_05() {
  check(
    r#"
val _ = 1.1 + 1
(**     ^^^^^^^ expected _ * _, found real * int *)
"#,
  );
}

#[test]
fn t_06() {
  check(
    r#"
val add = op+
val _ = add (false, true)
(**         ^^^^^^^^^^^^^ expected _ * _ with word, real, or int, found bool * bool *)
"#,
  );
}

#[test]
fn t_07() {
  check(
    r#"
val  _ = false + true
(**      ^^^^^^^^^^^^ expected _ * _ with word, real, or int, found bool * bool *)
"#,
  );
}
