//! Incomplete tokens.

use crate::check::check;

#[test]
fn num_lit() {
  check(
    r"
val _ = 0x
(**     ^^ missing digits in number literal *)
",
  );
}

#[test]
fn ty_var() {
  check(
    r"
datatype ' guh = no
(**      ^ incomplete type variable *)
",
  );
}
