//! Warnings about unused items.

use crate::check::check_with_warnings;

#[test]
fn smoke() {
  check_with_warnings(
    r"
fun f x = ()
(**   ^ unused value: `x` *)
",
  );
}

#[test]
fn or_both_used() {
  check_with_warnings(
    r"
datatype t = A of int | B of int

fun toInt (x : t) : int =
  let val (A y | B y) = x
  in y end
",
  );
}
