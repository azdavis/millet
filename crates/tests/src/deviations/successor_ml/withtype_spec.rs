//! Tests for `withtype` in specifications.

use crate::check::{check, check_multi, raw};

#[test]
fn default_disallow() {
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

#[test]
fn config_allow() {
  let config = r"
version = 1
language.successor-ml.sig-withtype = true
";
  let sml = r"
signature STREAM =
  sig
    datatype 'a u = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a u
  end
structure Stream : STREAM =
  struct
    datatype 'a u = Nil | Cons of 'a * (unit -> 'a u)
    type 'a t = unit -> 'a u
  end
";
  check_multi(raw::singleton(config, sml));
}
