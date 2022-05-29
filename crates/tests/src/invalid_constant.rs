use crate::check::check;

#[test]
fn char() {
  check(
    r#"
val _ = #"あ"
(**     ^^^^^^ character literal must have length 1 *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn int() {
  check(
    r#"
val _ = 123123123123123123123123132131
(**     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ invalid integer constant: number too large to fit in target type *)
"#,
  );
}

#[test]
fn real() {
  check(
    r#"
val _ = 123.
(**     ^^^^ incomplete literal *)
"#,
  );
}

#[test]
fn string() {
  check(
    r#"
val _ = "bad \ bad \ bad"
(**     ^^^^^^^ invalid string literal *)
"#,
  );
}
