use crate::check::check;

#[test]
fn smoke() {
  check(
    r#"
infix 0 a b c
infixr 0 d e f
nonfix g h i
"#,
  );
}

#[test]
fn negative() {
  check(
    r#"
infix ~3 bad
(**   ^^ fixity is negative *)
"#,
  );
}

#[test]
fn real() {
  check(
    r#"
infix 1.1 bad
(**   ^^^ expected a name or integer literal *)
"#,
  );
}

#[test]
fn word() {
  check(
    r#"
infix 0w1 bad
(**   ^^^ expected a name or integer literal *)
"#,
  );
}

#[test]
fn hex() {
  check(
    r#"
infix 0x1 bad
(**   ^^^ invalid fixity: invalid digit found in string *)
"#,
  );
}

#[test]
fn nonfix() {
  check(
    r#"
nonfix + = *

val _: bool = = (1, 2)
val _: int = + (1, 2)
val _: int = * (3, 4)
"#,
  );
}

#[test]
fn not_infix() {
  check(
    r#"
datatype t = C of int * int
fun f (_ C _) = 2
(**      ^ non-infix name used as infix *)
"#,
  );
}

#[test]
fn same_fixity_diff_assoc() {
  check(
    r#"
infix <<
infixr >>
val _ = 1 << 2 >> 3
(**            ^^ consecutive infix names with same fixity but different associativity *)
"#,
  );
}
