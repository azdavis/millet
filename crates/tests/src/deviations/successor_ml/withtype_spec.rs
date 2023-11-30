//! Tests for `withtype` in specifications.

use crate::check::check;

#[test]
fn disallow() {
  check(
    r"
signature STREAM =
  sig
    datatype 'a u = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a u
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ `withtype` in specifications *)
  end
",
  );
}
