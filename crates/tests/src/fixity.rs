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
(**   ^^^ expected a name *)
"#,
  );
}

#[test]
fn word() {
  check(
    r#"
infix 0w1 bad
(**   ^^^ expected a name *)
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
