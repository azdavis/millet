use crate::check::check;

#[test]
fn num_constant() {
  check(
    r#"
val _ = 0x
(**     ^^ incomplete numeric constant *)
"#,
  );
}

#[test]
fn ty_var() {
  check(
    r#"
datatype ' guh = no
(**      ^ incomplete type variable *)
"#,
  );
}
